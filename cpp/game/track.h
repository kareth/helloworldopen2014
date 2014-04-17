#ifndef CPP_GAME_TRACK_H_
#define CPP_GAME_TRACK_H_

#include <string>
#include <vector>

#include "game/piece.h"
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

  // The input json should point to the "track" part of the "gameInit"
  // command. E.g.
  //
  // {
  //   "id": ...,
  //   "name": ...,
  //   "pieces": []
  // }
  void ParseFromJson(const jsoncons::json& data);

  // TODO(tomek) iterator maybe?
 private:
  string id_;
  string name_;

  vector<Piece> pieces_;
};

}  // namespace game

#endif  // CPP_GAME_TRACK_H_
