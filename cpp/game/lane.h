#ifndef CPP_GAME_LANE_H_
#define CPP_GAME_LANE_H_

namespace game {

class Lane {
 public:
  Lane() {}
  Lane(int index, double distance) : index_(index), distance_from_center_(distance) {}

  int index() const { return index_; }
  double distance_from_center() const { return distance_from_center_; }

 private:
  double distance_from_center_;
  int index_;
};

}  // namespace game

#endif  // CPP_GAME_LANE_H_
