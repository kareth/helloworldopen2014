#include "game/car_tracker.h"

namespace game {

namespace {
template <typename T> int sgn(T val) {
    return (T(0) < val) - (val < T(0));
}
}  // anonymous namespace

CarTracker::CarTracker(const Race* race) : race_(race), lane_length_model_(&race_->track()) {
  stats_file_.open ("bin/stats.csv");
  stats_file_ << "piece_index,start_lane,end_lane,radius,in_piece_distance,angle,velocity,throttle" << std::endl;
}

CarTracker::~CarTracker() {
  stats_file_.close();
}

CarState CarTracker::Predict(const CarState& state, const Command& command) {
  TurboState turbo_state = state.turbo_state();
  double throttle = command.ThrottleSet() ? command.throttle() : state.throttle();
  double effective_throttle = turbo_state.is_on() ? throttle * turbo_state.turbo().factor() : throttle;
  if (command.TurboSet()) {
    turbo_state.Enable();
  }
  turbo_state.Decrement();

  double velocity = velocity_model_.Predict(state.velocity(), effective_throttle);
  double piece_distance = state.position().piece_distance() + velocity;
  int lap = state.position().lap();
  int piece = state.position().piece();
  int start_lane = state.position().start_lane();
  int end_lane = state.position().end_lane();
  Switch switch_state = state.switch_state();

  // Is it next piece?
  double lane_length = lane_length_model_.Length(state.position());
  if (piece_distance > lane_length) {
    piece_distance = piece_distance - lane_length;
    piece++;
    if (piece >= race_->track().pieces().size()) {
      piece = 0;
      lap++;
    }

    if (race_->track().pieces()[piece].has_switch() && switch_state != Switch::kStay) {
      // TODO Determine what is left and right lane
      if (switch_state == Switch::kSwitchRight) {
        end_lane = start_lane + 1;
        if (end_lane >= race_->track().lanes().size()) {
          std::cerr << "Changed lane out of track!" << std::endl;
          end_lane = race_->track().lanes().size() - 1;
        }
      } else {
        end_lane = start_lane - 1;
        if (end_lane < 0) {
          std::cerr << "Changed lane to -1!" << std::endl;
          end_lane = 0;
        }
      }
      switch_state = Switch::kStay;
    } else {
      start_lane = end_lane;
    }
  }
  switch_state = command.SwitchSet() ? command.get_switch() : switch_state;

  // TODO
  double radius = race_->track().LaneRadius(state.position().piece(), state.position().start_lane());
  double direction = -sgn(race_->track().pieces()[state.position().piece()].angle());

  // TODO
  //
  if (state.position().start_lane() != state.position().end_lane() && radius > 1e-12)
    radius = radius * 0.9;

  double angle = drift_model_.Predict(
      state.position().angle(),
      state.previous_angle(),
      state.velocity(),
      radius,
      direction);

  Position position;
  position.set_piece_distance(piece_distance);
  position.set_lap(lap);
  position.set_piece(piece);
  position.set_start_lane(start_lane);
  position.set_end_lane(end_lane);
  position.set_angle(angle);

  return CarState(position, velocity, state.position().angle(), switch_state, throttle, turbo_state);
}

void CarTracker::Record(const Position& position) {
  if (just_started_) {
    just_started_ = false;
    state_ = CarState(position);
  }

  // TODO Make sure everything works on crash.
  // TODO If velocity model is trained, it is probably better to use it for
  // velocity (because of unknown length of switches.
  // TODO Handle bumps with other cars, do not break models then.

  TurboState turbo_state = state_.turbo_state();
  double throttle = last_command_.ThrottleSet() ? last_command_.throttle() : state_.throttle();
  double effective_throttle = turbo_state.is_on() ? throttle * turbo_state.turbo().factor() : throttle;
  if (last_command_.TurboSet()) {
    turbo_state.Enable();
  }
  turbo_state.Decrement();

  Switch switch_state = last_command_.SwitchSet() ? last_command_.get_switch() : state_.switch_state();

  double velocity = 0;
  if (state_.position().piece() == position.piece()) {
    velocity = position.piece_distance() - state_.position().piece_distance();
  } else {
    bool lane_length_perfect = false;
    velocity = position.piece_distance() - state_.position().piece_distance() +
      lane_length_model_.Length(state_.position(), &lane_length_perfect);

    // Note: because of switches, it is better to use our model.
    if (!lane_length_perfect && velocity_model_.IsReady()) {
      velocity = velocity_model_.Predict(state_.velocity(), effective_throttle);
    }

    if (position.start_lane() != position.end_lane()) {
      switch_state = Switch::kStay;
    }
  }

  double direction = -sgn(race_->track().pieces()[state_.position().piece()].angle());

  // Update models
  crash_model_.RecordSafeAngle(position.angle());
  // There is too many problems in between pieces (length of switches),
  // so do not take those measurements into account.
  if (state_.position().piece() == position.piece()) {
    velocity_model_.Record(velocity, state_.velocity(), effective_throttle);
  }
  drift_model_.Record(
      position.angle(), state_.position().angle(), state_.previous_angle(),
      state_.velocity(), RadiusInPosition(state_.position()), direction);
  if (velocity_model_.IsReady()) {
    lane_length_model_.Record(state_.position(), position, velocity_model_.Predict(state_.velocity(), effective_throttle));
  }

  if (drift_model_.IsReady()) {
    crash_model_.RecordDriftModelReady();
  }

  state_ = CarState(position, velocity, state_.position().angle(), switch_state, throttle, turbo_state);
  LogState();
}

bool CarTracker::IsSafe(const CarState& state, const Command& command) {
  return IsSafe(Predict(state, command));
}

bool CarTracker::IsSafe(const CarState& state) {
  // TODO hardcoded
  double safe_speed = 3;

  auto s = state;
  while (s.velocity() > safe_speed) {
    if (!crash_model_.IsSafe(s.position().angle())) {
      return false;
    }
    s = Predict(s, Command(0));
  }
  return true;
}

bool CarTracker::IsReady() const {
  return velocity_model_.IsReady() && drift_model_.IsReady();
}

double CarTracker::RadiusInPosition(const Position& position) {
  return race_->track().LaneRadius(position.piece(), position.start_lane());
}

void CarTracker::LogState() {
  const auto& position = state_.position();

  stats_file_ << std::setprecision(std::numeric_limits<double>::digits10)
    << position.piece() << ","
    << position.start_lane() << ","
    << position.end_lane() << ","
    << race_->track().LaneRadius(position.piece(), position.start_lane()) << ","
    << position.piece_distance() << ","
    << state_.position().angle() << ","
    << state_.velocity() << ","
    << state_.throttle() << std::endl;
}

void CarTracker::RecordTurboAvailable(const game::Turbo& turbo) {
  state_.AddNewTurbo(turbo);
}

void CarTracker::RecordCarCrash() {
  auto crashed_state = Predict(state_, last_command_);
  crash_model_.RecordCarCrash(crashed_state.position().angle());
  Reset();
  stats_file_ << "CRASH" << std::endl;
}

}  // namespace game
