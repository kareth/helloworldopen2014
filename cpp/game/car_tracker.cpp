#include "game/car_tracker.h"

namespace game {

CarTracker::CarTracker(const Race* race) : race_(race) {
  stats_file_.open ("bin/stats.csv");
  stats_file_ << "piece_index,start_lane,end_lane,radius,in_piece_distance,angle,velocity,throttle" << std::endl;

  if (race_->track().id() == "germany" ||
      race_->track().id() == "keimola" ||
      race_->track().id() == "usa" ||
      race_->track().id() == "france") {
    drift_model_[0].reset(new DriftModel(0));
    drift_model_[0]->AddModel({1.9, -0.9, -0.00125, 0});
    drift_model_[1].reset(new DriftModel(1));
    drift_model_[1]->AddModel({1.9, -0.9, -0.00125, 0.00125});
    drift_model_[-1].reset(new DriftModel(-1));
    drift_model_[-1]->AddModel({1.9, -0.9, -0.00125, -0.00125});
  }
}

CarTracker::~CarTracker() {
  stats_file_.close();
}

CarState CarTracker::Predict(const CarState& state, const Command& command) {
  if (!IsReady()) {
    return state;
  }

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
  Switch switch_state = command.SwitchSet() ? command.get_switch() : state.switch_state();

  // Is it next piece?
  double lane_length = race_->track().LaneLength(state.position());
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
      } else {
        end_lane = start_lane - 1;
      }
      switch_state = Switch::kStay;
    } else {
      start_lane = end_lane;
    }
  }

  double radius = race_->track().LaneRadius(state.position().piece(), state.position().start_lane());
  // TODO
  if (state.position().start_lane() != state.position().end_lane() && radius > 1e-12)
    radius = radius * 0.9;

  double angle = GetDriftModel(state.position())->Predict(
      state.position().angle(),
      state.previous_angle(),
      state.velocity(),
      radius);

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
  // TODO Make sure everything works on crash.
  // TODO If velocity model is trained, it is probably better to use it for
  // velocity (because of unknown length of switches.
  // TODO Handle bumps with other cars, do not break models then.

  double velocity = 0;
  if (state_.position().piece() == position.piece()) {
    velocity = position.piece_distance() - state_.position().piece_distance();
  } else {
    velocity = position.piece_distance() - state_.position().piece_distance() +
      race_->track().LaneLength(state_.position());
  }

  // Update models
  crash_model_.Record(position.angle());
  velocity_model_.Record(velocity, state_.velocity(), last_command_.throttle());
  GetDriftModel(state_.position())->Record(
      position.angle(), state_.position().angle(), state_.previous_angle(),
      state_.velocity(), RadiusInPosition(state_.position()));

  Switch switch_state = last_command_.SwitchSet() ? last_command_.get_switch() : state_.switch_state();
  double throttle = last_command_.ThrottleSet() ? last_command_.throttle() : state_.throttle();
  TurboState turbo_state = state_.turbo_state();
  if (last_command_.TurboSet()) {
    turbo_state.Enable();
  }
  turbo_state.Decrement();

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
    if (s.position().angle() > 60 - 1e-9)
      return false;
    s = Predict(s, Command(0));
  }
  return true;
}

bool CarTracker::IsReady() const {
  return velocity_model_.IsReady();
}

DriftModel* CarTracker::GetDriftModel(const Position& position) {
  auto& piece = race_->track().pieces().at(position.piece());
  int direction = sgn(piece.angle());
  if (drift_model_[direction] == nullptr) {
    drift_model_[direction].reset(new DriftModel(direction));
  }
  return drift_model_[direction].get();
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
    << last_command_.throttle() << std::endl;
}

void CarTracker::RecordTurboAvailable(const game::Turbo& turbo) {
  state_.AddNewTurbo(turbo);
}

}  // namespace game
