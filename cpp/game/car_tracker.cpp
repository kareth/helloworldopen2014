#include "game/car_tracker.h"
#include "gflags/gflags.h"

DECLARE_string(race_id);

namespace game {

namespace {
template <typename T> int sgn(T val) {
    return (T(0) < val) - (val < T(0));
}
}  // anonymous namespace

CarTracker::CarTracker(const Race* race, const PhysicsParams& params)
    : race_(race),
      lane_length_model_(&race_->track(), params.switch_length_params),
      radius_model_(&race_->track(), &lane_length_model_, params.switch_radius_params),
      velocity_model_(params.velocity_model_params),
      drift_model_(params.drift_model_params) {

  params.LogMissingData(race->track());
}

CarTracker::~CarTracker() {
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

  // Return distance when starting from initial velocity and using throttles
double CarTracker::PredictDistance(const CarState& state, 
    const vector<double>& throttles) const {
  TurboState turbo_state = state.turbo_state();
  double velocity = state.velocity();

  double distance = 0;
  for (double throttle : throttles) {
    if (turbo_state.is_on()) {
      throttle *= turbo_state.turbo().factor();
      turbo_state.Decrement();
    }
    velocity = velocity_model_.Predict(velocity, throttle);
    distance += velocity;
  }
  return distance;
}

// Return distance when starting from initial velocity and using throttles how_many times
double CarTracker::PredictDistance(const CarState& state, int how_many, 
    double throttle) const {
  return PredictDistance(state, vector<double>(how_many, throttle));
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

bool CarTracker::IsSafe(const CarState& state, double safe_speed) {
  auto s = state;
  for (int i = 0; i < 200 && s.velocity() > safe_speed; ++i) {
    if (!crash_model_.IsSafe(s.position().angle())) {
      return false;
    }
    s = Predict(s, Command(0));
  }
  return true;
}

bool CarTracker::GenerateSafeStates(const CarState& state, vector<CarState>* states) {
  for (int i = 0; i < 15; ++i) {
    vector<CarState> tmp;
    if (!InternalAddStates(state, Command(1), i, &tmp)) {
      break;
    }
    if (InternalGenerateSafeStates(tmp.size() > 0 ? tmp.back() : state, &tmp)) {
      if (states) states->insert(states->end(), tmp.begin(), tmp.end());
      return true;
    }
  }
  return false;
}

bool CarTracker::InternalGenerateSafeStates(const CarState& state, vector<CarState>* states) {
  auto s = state;
  for (int i = 0; i < 100; ++i) {
    if (!crash_model_.IsSafe(s.position().angle())) {
      return false;
    }
    if (s.velocity() < 3 &&
        fabs(s.position().angle() - s.previous_angle()) < 2 &&
        s.position().angle() < 10) {
      break;
    }
    s = Predict(s, Command(0));
    if (states) states->push_back(s);
  }
  return true;
}

bool CarTracker::InternalAddStates(const CarState& state, Command command, int count, vector<CarState>* states) {
  auto s = state;
  for (int i = 0; i < count; ++i) {
    s = Predict(s, command);
    if (states) states->push_back(s);
    if (!crash_model_.IsSafe(s.position().angle())) {
      return false;
    }
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
}

void CarTracker::RecordTurboAvailable(const game::Turbo& turbo) {
  state_.AddNewTurbo(turbo);
}

void CarTracker::RecordCarCrash() {
  auto crashed_state = Predict(state_, last_command_);
  crash_model_.RecordCarCrash(crashed_state.position().angle());
  Reset();
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

double CarTracker::DistanceBetween(const Position& position1, const Position& position2, bool* is_perfect, double max_distance) const {
  bool tmp_perfect = true;
  if (is_perfect != nullptr) *is_perfect = true;
  double distance = 0.0;
  Position position = position1;
  // We could add while(true) but for safety we use const number of iterations.
  for (int i = 0; i < kDistanceBetweenIter; ++i) {
    if (distance >= max_distance) break;
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

Position CarTracker::PredictPosition(const Position& position, double distance, int target_lane) const {
  if (target_lane == -1) target_lane = position.end_lane();

  Position result = position;
  result.set_piece_distance(result.piece_distance() + distance);

  // Note: We should use while(true) here, but it is safer to
  // iterate const times.
  for (int i = 0; i < race_->track().pieces().size() + 1; ++i) {
    double lane_length = lane_length_model_.Length(result);

    if (result.piece_distance() < lane_length) {
      break;
    }

    result.set_piece_distance(result.piece_distance() - lane_length);
    result.set_piece(result.piece() + 1);
    if (result.piece() >= race_->track().pieces().size()) {
      result.set_piece(0);
      result.set_lap(result.lap() + 1);
    }

    result.set_start_lane(result.end_lane());

    if (race_->track().pieces()[result.piece()].has_switch() &&
        result.end_lane() != target_lane) {
      if (result.end_lane() < target_lane) {
        result.set_end_lane(result.end_lane() + 1);
      } else {
        result.set_end_lane(result.end_lane() - 1);
      }
    }
  }
  return result;
}

bool CarTracker::MinVelocity(const CarState& car_state, int ticks, const Position& target, double* min_velocity, int* full_throttle_ticks) const {
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

bool CarTracker::MaxVelocity(const CarState& enemy_state, const CarState& my_state, int ticks, double* max_velocity) {
  *max_velocity = 0.0;
  bool ok = false;
  for (int i = 0; i <= ticks; ++i) {
    for (int j = 0; i + j <= ticks; ++j) {
      int k = ticks - i - j;

      vector<double> throttles;
      for (int z = 0; z < i; ++z) throttles.push_back(1);
      for (int z = 0; z < j; ++z) throttles.push_back(0);
      for (int z = 0; z < k; ++z) throttles.push_back(1);

      bool all_safe = true;
      CarState tmp = enemy_state;
      for (int z = 0; z < (int)throttles.size() - 1; z++) {
        tmp = Predict(tmp, Command(throttles[z]));
        if (!crash_model_.IsSafe(tmp.position().angle())) { all_safe = false; break; }
      }
      CarState t = Predict(tmp, Command(throttles.back()));
      if (!crash_model_.IsSafe(t.position().angle())) { all_safe = false; }
      if (!all_safe) continue;

      if (!HasBumped(tmp, my_state) && HasBumped(t, my_state)) {
        ok = true;
        *max_velocity = fmax(*max_velocity, t.velocity());
      }
    }
  }
  return ok;
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
    curves.push_back(Curve(-sgn(previous_piece.angle()), radius_model_.Radius(position), 0));
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

    curves.push_back(Curve(-sgn(piece.angle()), radius_model_.Radius(position), d));

    d += lane_length_model_.Length(position);
    previous_piece = piece;
  }
  //TODO(Wojtek): Not very elegant
  for (int i=0; i<curves.size()-1;++i) {
      curves[i].length = curves[i+1].distance - curves[i].distance;
  }
  curves.back().length = 10000; //TODO(Wojtek): Not very elegant

  return curves;
}

PhysicsParams CarTracker::CreatePhysicsParams() {
  PhysicsParams physics_params;
  physics_params.velocity_model_params = velocity_model_.CreateParams();
  physics_params.drift_model_params = drift_model_.CreateParams();
  physics_params.switch_length_params = lane_length_model_.CreateParams();
  physics_params.switch_radius_params = radius_model_.CreateParams();
  return physics_params;
}

// Note: This method is slow and performance can be improved in many places.
bool CarTracker::CanBumpAfterNTicks(
    const CarState& my_state,
    const CarState& enemy_state,
    int ticks_after,
    double* min_velocity) const {
  // The minimum and maximum distance enemy can travel.
  double enemy_min_distance = 1000000000.0;
  double enemy_max_distance = 0.0;
  for (int full_throttle_ticks = 0; full_throttle_ticks <= ticks_after; full_throttle_ticks += ticks_after) {
    double v = enemy_state.velocity();
    double d = 0.0;
    for (int i = 0; i < full_throttle_ticks; ++i) { v = velocity_model_.Predict(v, 1); d += v; }
    for (int i = 0; i < ticks_after - full_throttle_ticks; ++i) { v = velocity_model_.Predict(v, 0); d += v; }

    if (full_throttle_ticks == 0) {
      enemy_min_distance = d;
    } else {
      enemy_max_distance = d;
    }
  }
  // std::cout << "ticks_after: " << ticks_after << std::endl;
  // std::cout << "enemy_position: " << enemy_state.position().ShortDebugString() << std::endl;
  // std::cout << "enemy_min_distance: " << enemy_min_distance << std::endl
  //           << "enemy_max_distance: " << enemy_max_distance << std::endl;

  const double kCarLength = race_->cars().at(0).length();

  Position min_position = my_state.position();
  // NOTE: Not passing target lane is intentional beucase bumping on switches
  // is strange. If we are just before the switch, we can only bump into people
  // that are not changing lanes on the switch. If someone is changing the lane
  // then bump will not happen.
  Position max_position = PredictPosition(my_state.position(), kCarLength);

  // std::cout << "min_position: " << min_position.ShortDebugString() << std::endl
  //           << "max_position: " << max_position.ShortDebugString() << std::endl;

  // We substract the distance in min_position instead of using DistanceBetween,
  // because of standing cars. If cars stands exactly between min and max, then
  // distance from enemy position to min_position would be huge (and we don't want
  // that).
  double distance_to_max_position = DistanceBetween(enemy_state.position(), max_position);
  double distance_to_min_position = distance_to_max_position - kCarLength;

  // std::cout << "distance_to_min_position: " << distance_to_min_position << std::endl
  //           << "distance_to_max_position: " << distance_to_max_position << std::endl;

  // Check if the enemy can even get to the bumping range using 'ticks_after'.
  if (enemy_max_distance < distance_to_min_position ||
      distance_to_max_position < enemy_min_distance) {
    // std::cout << "It is NOT possible for enemy to get to bumping range" << std::endl;
    return false;
  }

  // std::cout << "It is possible for enemy to get to bumping position" << std::endl;

  // This is a special case for switches.
  if (enemy_max_distance < distance_to_max_position) {
    if (DistanceBetween(min_position, PredictPosition(enemy_state.position(), enemy_max_distance, max_position.end_lane()), nullptr, kCarLength + 1) >= kCarLength + 1e-9) {
      // std::cout << "He is on the switch but he can't bump me" << std::endl;
      return false;
    }
  }

  for (int full_throttle_ticks = 0; full_throttle_ticks <= ticks_after; full_throttle_ticks++) {
    double v = enemy_state.velocity();
    double d = 0.0;
    for (int i = 0; i < full_throttle_ticks; ++i) { v = velocity_model_.Predict(v, 1); d += v; }
    for (int i = 0; i < ticks_after - full_throttle_ticks; ++i) { v = velocity_model_.Predict(v, 0); d += v; }

    if (d < distance_to_min_position || full_throttle_ticks == 0) {
      *min_velocity = v;
    }
  }

  if (*min_velocity > my_state.velocity()) {
    // std::cout << "Enemy has higher velocity than we (he was probably behind us)" << std::endl;
    return false;
  }

  return true;
}

bool CarTracker::IsBumpInevitable(
    const CarState& my_state_before,
    const CarState& my_state_after,
    const CarState& enemy_state,
    int ticks_after) const {
  const double kCarLength = race_->cars().at(0).length();
  double v = enemy_state.velocity();
  double enemy_distance = 0.0;
  for (int i = 0; i < ticks_after; ++i) {
    v = velocity_model_.Predict(v, 1);
    enemy_distance += v;
  }

  // Can change lanes within distance?
  int target_lane = enemy_state.position().end_lane();
  if (target_lane > 0) target_lane--;
  else target_lane++;

  Position bump_position = PredictPosition(my_state_after.position(), kCarLength);

  Position enemy_position_after = PredictPosition(enemy_state.position(), enemy_distance, target_lane);
  // He was able to switch lane.
  if (enemy_position_after.start_lane() == target_lane) {
    // std::cout << "Enemy is able to switch lane" << std::endl;
    return false;
  }

  double my_distance = DistanceBetween(my_state_before.position(), bump_position);

  double check_distance =
      DistanceBetween(my_state_before.position(), enemy_state.position(), nullptr, my_distance) +
      enemy_distance +
      DistanceBetween(enemy_position_after, bump_position, nullptr, my_distance);

  if (fabs(my_distance - check_distance) < 1e-5) {
    return true;
  }

  // std::cout << "my_state_before: " << my_state_before.position().ShortDebugString() << std::endl;
  // std::cout << "bump_position: " << bump_position.ShortDebugString() << std::endl;
  // std::cout << "enemy_position: " << enemy_state.position().ShortDebugString() << std::endl;
  // std::cout << "enemy_position_after: " << enemy_position_after.ShortDebugString() << std::endl;

  // std::cout << "my_distance: " << my_distance << std::endl;
  // std::cout << "enemy_distance: " << enemy_distance << std::endl;
  // std::cout << "check_distance: " << check_distance << std::endl;

  return false;
}

bool CarTracker::HasBumped(
    const CarState& state1,
    const CarState& state2) {
  const double kCarLength = race_->cars()[0].length();

  if (state1.position().start_lane() != state2.position().start_lane() ||
      state1.position().end_lane() != state2.position().end_lane()) {
    return false;
  }

  return DistanceBetween(state1.position(), state2.position(), nullptr, kCarLength + 1) <= kCarLength + 1e-9 ||
         DistanceBetween(state2.position(), state1.position(), nullptr, kCarLength + 1) <= kCarLength + 1e-9;
}

bool CarTracker::IsSafeAttack(
    const CarState& my_state,
    const CarState& current_enemy_state,
    Command* command,
    bool allow_turbo) {
  CarState enemy_state = current_enemy_state;

  if (DistanceBetween(my_state.position(), enemy_state.position()) > 200) {
    return false;
  }

  if (allow_turbo && !my_state.turbo_state().available()) {
    std::cerr << "ERROR: Trying to attack wit turbo when not available" << std::endl;
    return false;
  }

  // Assume that enemy will not switch lanes.
  Command command1;
  if (!IsSafeAttackWithoutSwitches(my_state, enemy_state, &command1, allow_turbo)) {
    return false;
  }

  // TODO In theory we don't need to simulate below if enemy couldn't perform
  // switch operation.

  // If enemy can change lane to left, simulate that.
  Command command2 = command1;
  if (enemy_state.position().end_lane() > 0) {
    enemy_state.set_switch_state(Switch::kSwitchLeft);
    if (!IsSafeAttackWithoutSwitches(my_state, enemy_state, &command2, allow_turbo)) {
      std::cout << "IsSafeAttack: If enemy switches to left, attack would be unsuccessful." << std::endl;
      return false;
    }
  }

  // If enemy can change lane to right, simulate that.
  Command command3 = command1;
  if (enemy_state.position().end_lane() + 1 < race_->track().lanes().size()) {
    enemy_state.set_switch_state(Switch::kSwitchRight);
    if (!IsSafeAttackWithoutSwitches(my_state, enemy_state, &command3, allow_turbo)) {
      std::cout << "IsSafeAttack: If enemy switches to right, attack would be unsuccessful." << std::endl;
      return false;
    }
  }

  if (command1 == command2 && command2 == command3) {
    *command = command1;
    return true;
  }

  std::cout << "Simulating different switches returned different commands." << std::endl;
  return false;
}

bool CarTracker::IsSafeAttackWithoutSwitches(
    const CarState& current_state,
    const CarState& current_enemy_state,
    Command* command,
    bool allow_turbo) {
  *command = Command(1.0);

  CarState my_state = current_state;
  CarState enemy_state = current_enemy_state;

  vector<CarState> enemy_safe_states{enemy_state};
  if (!GenerateSafeStates(enemy_state, &enemy_safe_states)) {
    std::cout << "IsSafeAttack: Cannot find safe states for enemy. Attack not worth it" << std::endl;
    return false;
  }

  bool safe_state_bumped = false;

  for (int ticks_after = 1; ticks_after < enemy_safe_states.size(); ++ticks_after) {
    // Compute follow command.
    Command follow_command = Command(1.0);
    if (my_state.position().end_lane() < enemy_state.position().end_lane() && my_state.switch_state() != Switch::kSwitchRight) {
      follow_command = Command(Switch::kSwitchRight);
    }
    if (my_state.position().end_lane() > enemy_state.position().end_lane() && my_state.switch_state() != Switch::kSwitchLeft) {
      follow_command = Command(Switch::kSwitchLeft);
    }
    if (allow_turbo && !my_state.turbo_state().is_on() && ticks_after == 1) follow_command = Command::Turbo();

    if (ticks_after == 1) *command = follow_command;
    my_state = Predict(my_state, follow_command);
    enemy_state = Predict(enemy_state, Command(1));

    if (DistanceBetween(my_state.position(), enemy_state.position(), nullptr, 201) > 200) {
      std::cout << "IsSafeAttack: Can't attack because we applied switch." << std::endl;
      return false;
    }

    if (!crash_model_.IsSafe(my_state.position().angle()) ||
        !crash_model_.IsSafe(enemy_state.position().angle())) {
      return false;
    }

    // TODO We check if enemy crashes only at first and last bump assuming that
    // in all states in between he will crash. Consider checking this in all
    // intermiediate states.
    const CarState& enemy_safe_state = enemy_safe_states[ticks_after];
    if (!safe_state_bumped && HasBumped(my_state, enemy_safe_state)) {
      safe_state_bumped = true;
      CarState tmp = my_state;
      tmp.set_velocity(enemy_safe_state.velocity() * 0.8);
      if (!GenerateSafeStates(tmp, nullptr)) {
        std::cout << "Attack bump would crash me (first bump). Not worth it" << std::endl;
        return false;
      }
      tmp = enemy_safe_state;
      tmp.set_velocity(my_state.velocity() * 0.9);
      if (GenerateSafeStates(tmp, nullptr)) {
        std::cout << "Attack bump would not crash enemy (first bump). Not worth it" << std::endl;
        return false;
      }
    }

    // This is last possible bump.
    if (HasBumped(my_state, enemy_state)) {
      std::cout << "Final bump after " << ticks_after << " ticks" << std::endl;
      std::cout << "my_state: " << my_state.ShortDebugString() << std::endl;
      std::cout << "enemy_state: " << enemy_state.ShortDebugString() << std::endl;
      CarState tmp = my_state;
      tmp.set_velocity(enemy_state.velocity() * 0.8);
      if (!GenerateSafeStates(tmp, nullptr)) {
        std::cout << "Attack bump would crash me (last bump). Not worth it" << std::endl;
        return false;
      }
      tmp = enemy_state;
      tmp.set_velocity(my_state.velocity() * 0.9);
      if (GenerateSafeStates(tmp, nullptr)) {
        std::cout << "Attack bump would not crash enemy (last bump). Not worth it" << std::endl;
        return false;
      }
      return true;
    }

    if (safe_state_bumped) {
      double velocity;
      if (CanBumpAfterNTicks(my_state, current_enemy_state, ticks_after, &velocity)) {
        CarState tmp = my_state;
        tmp.set_velocity(velocity * 0.8);
        if (!GenerateSafeStates(tmp, nullptr)) {
          std::cout << "Attack bump would crash me (middle bump). Not worth it" << std::endl;
          return false;
        }
      } else {
        std::cerr << "ERROR: CanBumpAfterNTicks says bump is impossible, even though it was possible!" << std::endl;
      }
    }
  }

  std::cout << "Finished simulation." << std::endl;
  return false;
}

}  // namespace game
