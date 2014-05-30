#include "game/car_tracker.h"
#include "gflags/gflags.h"

DECLARE_string(race_id);

namespace game {

namespace {
template <typename T> int sgn(T val) {
    return (T(0) < val) - (val < T(0));
}
}  // anonymous namespace

CarTracker::CarTracker(const Race* race)
  : race_(race), radius_model_(&race_->track()), lane_length_model_(&race_->track()) {
  stats_file_.open ("bin/" + FLAGS_race_id + "/stats.csv");
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
  switch_state = command.SwitchSet() ? command.get_switch() : switch_state;

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

  double radius = radius_model_.Radius(state.position());
  double direction = -sgn(race_->track().pieces()[state.position().piece()].angle());
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

void CarTracker::Record(const Position& position, bool bump) {
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

  if (!bump && !last_record_had_bump) {
    double direction = -sgn(race_->track().pieces()[state_.position().piece()].angle());

    // Update models
    crash_model_.RecordSafeAngle(position.angle());
    // There is too many problems in between pieces (length of switches),
    // so do not take those measurements into account.
    if (state_.position().piece() == position.piece()) {
      velocity_model_.Record(velocity, state_.velocity(), effective_throttle);

      // Do not learn drift model on switches.
      if (state_.position().start_lane() == state_.position().end_lane()) {
        drift_model_.Record(
            position.angle(), state_.position().angle(), state_.previous_angle(),
            state_.velocity(), RadiusInPosition(state_.position()), direction);
      }
    }

    if (velocity_model_.IsReady()) {
      lane_length_model_.Record(state_.position(), position, velocity_model_.Predict(state_.velocity(), effective_throttle));
    }

    if (drift_model_.IsReady()) {
      crash_model_.RecordDriftModelReady();
      double r = drift_model_.EstimateRadius(position.angle(), state_.position().angle(), state_.previous_angle(),
          state_.velocity(), direction);

      double expected_angle = drift_model_.Predict(state_.position().angle(), state_.previous_angle(), state_.velocity(), r, direction);
      radius_model_.Record(state_.position(), r);
    }
  } else {
    // std::cout << "Someone probably bumped, do not learn models" << std::endl;
  }

  state_ = CarState(position, velocity, state_.position().angle(), switch_state, throttle, turbo_state);
  LogState();
  last_record_had_bump = bump;
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

CarState CarTracker::CreateCarState(const CarState& prev, const Position& position) const {
  double velocity = 0.0;
  if (prev.position().piece() == position.piece()) {
    velocity = position.piece_distance() - prev.position().piece_distance();
  } else {
    velocity = position.piece_distance() - prev.position().piece_distance() +
      lane_length_model_.Length(prev.position());
  }

  TurboState turbo_state = prev.turbo_state();
  turbo_state.Decrement();

  return CarState(position, velocity, prev.position().angle(), Switch::kStay, 0.0, turbo_state);
}

double CarTracker::DistanceBetween(const Position& position1, const Position& position2, bool* is_perfect) {
  bool tmp_perfect = true;
  if (is_perfect != nullptr) *is_perfect = true;
  double distance = 0.0;
  Position position = position1;
  // We could add while(true) but for safety we use const number of iterations.
  for (int i = 0; i < kDistanceBetweenIter; ++i) {
    if (position.piece() == position2.piece() &&
        position.piece_distance() <= position2.piece_distance() &&
        position.start_lane() == position2.start_lane()) {

      if (position.piece_distance() != 0.0 && position.end_lane() != position2.end_lane()) {
      } else {
        distance += position2.piece_distance() - position.piece_distance();
        break;
      }
    }

    double lane_length = lane_length_model_.Length(position, &tmp_perfect);
    if (is_perfect != nullptr) *is_perfect = (*is_perfect) && tmp_perfect;
    distance += lane_length - position.piece_distance();

    position.set_piece_distance(0);
    position.set_piece((position.piece() + 1) % race_->track().pieces().size());
    position.set_start_lane(position.end_lane());
    if (race_->track().pieces()[position.piece()].has_switch()) {
      if (position.end_lane() < position2.start_lane()) {
        position.set_end_lane(position.end_lane() + 1);
      }
      if (position.end_lane() > position2.start_lane()) {
        position.set_end_lane(position.end_lane() - 1);
      }
    }
  }
  return distance;
}

Position CarTracker::PredictPosition(const Position& position, double distance) {
  double piece_distance = position.piece_distance() + distance;
  double lane_length = lane_length_model_.Length(position);
  int piece = position.piece();

  if (piece_distance > lane_length) {
    piece_distance = piece_distance - lane_length;
    piece++;
    if (piece >= race_->track().pieces().size()) {
      piece = 0;
    }
  }

  Position result = position;
  result.set_piece_distance(piece_distance);
  result.set_piece(piece);
  return result;
}

bool CarTracker::MinVelocity(const CarState& car_state, int ticks, const Position& target, double* min_velocity, int* full_throttle_ticks) {
  // TODO we should improve this method!
  double distance = DistanceBetween(car_state.position(), target);
  if (distance > 1000) return false;

  // std::cout << "Distance to reach: " << distance << " in " << ticks << " ticks" << std::endl;

  bool smaller = false;
  bool greater = false;
  for (int full_throttle_ticks = 0; full_throttle_ticks < ticks; ++full_throttle_ticks) {
    double v = car_state.velocity();
    double d = 0.0;
    for (int i = 0; i < full_throttle_ticks; ++i) { d += v; v = velocity_model_.Predict(v, 1); }
    for (int i = 0; i < ticks - full_throttle_ticks; ++i) { d += v; v = velocity_model_.Predict(v, 0); }
    if (d < distance) {
      smaller = true;
      *min_velocity = v;
    }
    if (d > distance) {
      greater = true;
    }
  }

  return smaller && greater;
}

bool CarTracker::HasSomeoneMaybeBumpedMe(const map<string, Position>& positions, const std::string& color) {
  const double kCarLength = race_->cars().at(0).length();

  for (const auto& p : positions) {
    if (p.first == color) continue;

    // We are adding 10 if the distance was not perfect, because
    // it is possible we do not have the length of the switch
    // yet.

    bool perfect = false;
    double d = DistanceBetween(positions.at(color), p.second, &perfect);
    if (d < kCarLength + 1e-9 + (perfect ? 0 : 10.0)) {
      return true;
    }

    perfect = false;
    d = DistanceBetween(p.second, positions.at(color), &perfect);
    if (d < kCarLength + 1e-9 + (perfect ? 0 : 10.0)) {
      return true;
    }
  }

  return false;
}

bool CarTracker::BoundaryThrottle(const CarState& car_state, double* throttle) {
  // Switches have different radiuses anyway.
  if (car_state.position().start_lane() != car_state.position().end_lane()) {
    return false;
  }

  const auto& piece = race_->track().PieceFor(car_state.position());
  const auto& next_piece = race_->track().PieceFor(car_state.position(), 1);

  // TODO(tomek): We assume pieces are long enough we cant skip them
  if (piece == next_piece) {
    *throttle = 1;
    return true;
  }

  double distance = lane_length_model_.Length(car_state.position()) - car_state.position().piece_distance();
  return velocity_model_.BoundaryThrottle(car_state.velocity(), distance, throttle);
}

// TODO(tomek): make it working for switches
vector<CarTracker::Curve> CarTracker::GetCurves(const CarState& car_state, double distance) {
  double d = 0.0;

  vector<Curve> curves;
  if (car_state.position().start_lane() != car_state.position().end_lane()) {
    return curves;
  }

  Position position = car_state.position();

  Piece previous_piece = race_->track().PieceFor(car_state.position(), 0);
  {
    d += lane_length_model_.Length(car_state.position()) - car_state.position().piece_distance();
    curves.push_back(Curve(-sgn(previous_piece.angle()), radius_model_.Radius(position), 0, d));
  }

  // Consider maximum of 100 pieces.
  for (int i = 1; i < 100; ++i) {
    if (d > distance) {
      break;
    }

    Piece piece = race_->track().PieceFor(car_state.position(), i);

    position.set_piece((position.piece() + 1) % race_->track().pieces().size());

    if (piece == previous_piece) {
      d += lane_length_model_.Length(position);
      previous_piece = piece;
      continue;
    }

    curves.push_back(Curve(-sgn(piece.angle()), radius_model_.Radius(position), d, d - curves.back().distance));

    d += lane_length_model_.Length(position);
    previous_piece = piece;
  }

  return curves;
}

}  // namespace game
