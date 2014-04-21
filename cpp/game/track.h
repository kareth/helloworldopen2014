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

  double Distance(const Position& position, const Position& previous) const;
  double LaneRadius(int piece, int lane) const;
  double LaneLength(int piece, int lane) const;

  const string& id() const { return id_; }
  const string& name() const { return name_; }
  const vector<Piece>& pieces() const { return pieces_; }
  const vector<Lane>& lanes() const { return lanes_; }

  // Returns current piece for car position.
  // offset returns current_index + offset
  const game::Piece& PieceFor(const game::Position& position, int offset = 0) const;

  // The input json should point to the "track" part of the "gameInit"
  // command. E.g.
  //
  // {
  //   "id": ...,
  //   "name": ...,
  //   "pieces": []
  // }
  void ParseFromJson(const jsoncons::json& data);


 private:
  string id_;
  string name_;

  vector<Piece> pieces_;
  vector<Lane> lanes_;
};

}  // namespace game

#endif  // CPP_GAME_TRACK_H_
