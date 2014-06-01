#include "game/velocity_predictor.h"

namespace game {

VelocityPredictor::VelocityPredictor(CarTracker& car_tracker, const Race& race)
  : car_tracker_(car_tracker), race_(race) {
}

void VelocityPredictor::Record(const CarState& state) {
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
    if (car_tracker_.DistanceBetween(point.position(), state.position()) <
        car_tracker_.DistanceBetween(state.position(), point.position()))
      break;

    auto new_velocity = InterpolatePoint(state_, state, point.position());
    if (new_velocity > point.velocity())
      points[state.position().end_lane()].erase(point);
  }

  AddPoint(state);
}

double VelocityPredictor::Velocity(const Position& p) {
  if (HasDataToPredict(p))
    return InterpolatePoint(Previous(p), Next(p), p);

  for (int off = 1; off < race_.track().lanes().size(); off++) {
    for (int side = -1; side <= 1; side+=2) {
      if (race_.track().IsLaneCorrect(p.end_lane() + off * side)) {
        // Approximate velocity from another lane
        auto pos = PositionOnAnotherLane(p, p.end_lane() + off * side);
        if (HasDataToPredict(pos))
          return InterpolatePoint(Previous(pos), Next(pos), p);
      }
    }
  }

  return state_.velocity();
}

void VelocityPredictor::AddPoint(const CarState& state) {
  points[state.position().end_lane()].insert(state);
  state_ = state;
}

double VelocityPredictor::InterpolatePoint(const CarState& a, const CarState& b, const Position& p) {
  double distance = car_tracker_.DistanceBetween(a.position(), b.position());
  double point_distance = car_tracker_.DistanceBetween(a.position(), p);
  return (b.velocity() - a.velocity()) * point_distance / distance + a.velocity();
}

bool VelocityPredictor::HasDataToPredict(const Position& p) {
  if (points[p.end_lane()].size() < 3) return false;

  auto previous = Previous(p);
  auto next = Next(p);
  if (car_tracker_.DistanceBetween(previous.position(), next.position()) <=
      next.velocity() + 1e-3) {
    return true;
  } else {
    return false;
  }
}

// Assumes that there is anything!
CarState VelocityPredictor::Next(Position p) {
  auto next = points[p.end_lane()].upper_bound(CarState(p));
  if (next == points[p.end_lane()].end())
    next = points[p.end_lane()].begin();

  return *next;
}

// Assumes that there is anything!
CarState VelocityPredictor::Previous(Position p) {
  auto previous = points[p.end_lane()].lower_bound(CarState(p));
  if (previous == points[p.end_lane()].begin())
    previous = points[p.end_lane()].end();
  previous--;

  return *previous;
}

// TODO lane_model
Position VelocityPredictor::PositionOnAnotherLane(const Position& p, int lane) {
  Position a = p;
  a.set_piece_distance(0);
  Position b = p;
  b.set_piece((b.piece() + 1) % race_.track().pieces().size());
  b.set_piece_distance(0);
  double unknown_lane_length = car_tracker_.DistanceBetween(a, b);

  a.set_end_lane(lane);
  b.set_end_lane(lane);
  double data_lane_length = car_tracker_.DistanceBetween(a, b);

  auto pos = p;
  pos.set_piece_distance(
      pos.piece_distance() / unknown_lane_length * data_lane_length);
  pos.set_end_lane(lane);
  return pos;
}

}  // namespace game
