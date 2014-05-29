#ifndef CPP_GAME_TRACK_H_
#define CPP_GAME_TRACK_H_

#include <string>
#include <vector>

#include "game/piece.h"
#include "game/lane.h"
#include "game/position.h"
#include "jsoncons/json.hpp"

using std::string;
using std::vector;

namespace game {

class Track {
 public:
  Track() {
  }

  const string& id() const { return id_; }
  const string& name() const { return name_; }
  const vector<Piece>& pieces() const { return pieces_; }
  const vector<Lane>& lanes() const { return lanes_; }

  // The input json should point to the "track" part of the "gameInit"
  // command. E.g.
  //
  // {
  //   "id": ...,
  //   "name": ...,
  //   "pieces": []
  // }
  void ParseFromJson(const jsoncons::json& data);

  // ================== HELPER METHODS BELOW ======================

  // TODO test
  bool IsLaneCorrect(int lane) const {
    return lane >= 0 && lane < lanes_.size();
  }

  // TODO remove

  // Returns current piece for car position.
  // offset returns current_index + offset
  const Piece& PieceFor(const Position& position, int offset = 0) const;
  bool IsFirstInFront(const Position& front, const Position& back) const;
  bool IsBetween(const Position& front, int piece1, int piece2) const;

  // Not including this one
  int NextSwitch(int piece_index) const;

  bool IsLastStraight(const Position& position) const;
  double LaneRadius(int piece, int lane) const;

 private:
  double LaneLength(int piece, int lane) const;
  string id_;
  string name_;

  vector<Piece> pieces_;
  vector<Lane> lanes_;
};

}  // namespace game

#endif  // CPP_GAME_TRACK_H_
