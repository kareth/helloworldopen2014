#include "game/velocity_predictor.h"

namespace game {

VelocityPredictor::VelocityPredictor(CarTracker& car_tracker, const Race& race)
  : car_tracker_(car_tracker), race_(race) {
}

namespace {

CarState Flatten(CarState state) {
  state.position_reference().set_start_lane(state.position().end_lane());
  return state;
}

Position Flatten(Position position) {
  position.set_start_lane(position.end_lane());
  return position;
}

}  // Anonymous

void VelocityPredictor::Reset(const CarState& raw_state) {
  auto state = Flatten(raw_state);
  // If new point or faster one;
  if (!HasDataToPredict(state.position()) ||
      Velocity(state.position()) < state.velocity()) {
    points[state.position().end_lane()].insert(state);
  }
  state_ = state;
}

void VelocityPredictor::Record(const CarState& raw_state) {
  if (IsOnExceedingSwitch(raw_state))
    return;

  auto state = Flatten(raw_state);

  // If the point is 'something new' or if we just swapped lanes
  if (!HasDataToPredict(state.position()) ||
      state.position().end_lane() != state_.position().end_lane()) {
    AddPoint(state);
    return;
  }

  // Check for all points between last point and current point and check if they are below
  // the line between aforementioned 2 points
  auto point = state_;
  while (true) {
    point = Next(point.position());

    if (car_tracker_.DistanceBetween(point.position(), state.position()) >
        car_tracker_.DistanceBetween(state.position(), point.position()))
      break;

    auto new_velocity = InterpolatePoint(state_, state, point.position());
    if (new_velocity > point.velocity())
      points[point.position().end_lane()].erase(point);
  }

  AddPoint(state);
}

double VelocityPredictor::Velocity(const Position& position) const {
  auto p = Flatten(position);

  if (HasDataToPredict(p))
    return InterpolatePoint(Previous(p), Next(p), p);

  for (int off = 1; off < race_.track().lanes().size(); off++) {
    for (int side = -1; side <= 1; side+=2) {
      if (race_.track().IsLaneCorrect(p.end_lane() + off * side)) {
        // Approximate velocity from another lane
        auto pos = PositionOnAnotherLane(p, p.end_lane() + off * side);
        if (HasDataToPredict(pos))
          return InterpolatePoint(Previous(pos), Next(pos), pos);
      }
    }
  }

  return state_.velocity();
}

void VelocityPredictor::AddPoint(const CarState& state) {
  points[state.position().end_lane()].insert(state);
  state_ = state;
}

double VelocityPredictor::InterpolatePoint(const CarState& a, const CarState& b, const Position& p) const {
  double distance = car_tracker_.DistanceBetween(a.position(), b.position());
  double point_distance = car_tracker_.DistanceBetween(a.position(), p);
  //printf ("%.2lf %.2lf\n", distance, point_distance);
  return (b.velocity() - a.velocity()) * point_distance / distance + a.velocity();
}

bool VelocityPredictor::HasDataToPredict(const Position& p) const {
  if (points[p.end_lane()].size() < 2) return false;

  auto previous = Previous(p);
  auto next = Next(p);
  if (car_tracker_.DistanceBetween(
        previous.position(), next.position(), nullptr, next.velocity() * 3) <=
      next.velocity() + 1e-3) {
    return true;
  } else {
    return false;
  }
}

// Assumes that there is anything!
CarState VelocityPredictor::Next(Position p) const {
  auto next = points[p.end_lane()].upper_bound(CarState(p));
  if (next == points[p.end_lane()].end())
    next = points[p.end_lane()].begin();

  return *next;
}

// Assumes that there is anything! (less or equal)
CarState VelocityPredictor::Previous(Position p) const {
  auto previous = points[p.end_lane()].upper_bound(CarState(p));
  if (previous == points[p.end_lane()].begin())
    previous = points[p.end_lane()].end();
  previous--;

  return *previous;
}

bool VelocityPredictor::IsOnExceedingSwitch(const CarState& state) {
  if (state.position().end_lane() != state.position().start_lane()) {
    auto pos = state.position();
    pos.set_start_lane(pos.end_lane());
    if (state.position().piece_distance() >
        car_tracker_.lane_length_model().Length(pos))
      return true;
  }
  return false;
}


Position VelocityPredictor::PositionOnAnotherLane(const Position& p, int lane) const {
  Position pos = p;

  pos.set_start_lane(pos.end_lane());
  double unknown_lane_length = car_tracker_.lane_length_model().Length(pos);

  pos.set_start_lane(lane);
  pos.set_end_lane(lane);
  double data_lane_length = car_tracker_.lane_length_model().Length(pos);

  pos.set_piece_distance(pos.piece_distance() / unknown_lane_length * data_lane_length);
  return pos;
}

void VelocityPredictor::PrintData() const {
  printf("Printing data: (lane, piece, position = speed)\n");
  printf("Current state: (%d %d %.2lf = %.2lf)\n", state_.position().end_lane(),
                                     state_.position().piece(),
                                     state_.position().piece_distance(),
                                     state_.velocity());
  for (int lane = 0; lane < race_.track().lanes().size(); lane++) {
    printf("Lane %d\n", lane);
    for (auto& s : points[lane]) {
      printf("(%d %.2lf = %.2lf)\n", s.position().piece(),
                                     s.position().piece_distance(),
                                     s.velocity());

    }
  }
}

}  // namespace game
