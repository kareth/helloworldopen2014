#ifndef CPP_GAME_TRACK_H_
#define CPP_GAME_TRACK_H_

#include <string>
#include <vector>

using std::string;
using std::vector;

namespace jsoncons {
class json;
}

namespace game {

class Lane;
class Piece;

class Track {
 public:
  Track(Track&) = delete;
  void operator=(Track) = delete;

  static std::unique_ptr<Track> ParseFromJson(const jsoncons::json& data);

  // TODO(tomek) iterator maybe?
 private:
  string id_;
  string name_;
  vector<Piece> pieces_;
  vector<Lane> lanes_;
};

}  // namespace game

#endif  // CPP_GAME_TRACK_H_
