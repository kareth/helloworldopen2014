#ifndef CPP_GAME_TRACK_H_
#define CPP_GAME_TRACK_H_

#include <string>
#include <vector>

using std::string;
using std::vector;

namespace game {

class Lane;
class Piece;

class Track {
 public:
   // TODO(tomek) iterator maybe?
 private:
   string id_;
   string name_;
   vector<Piece> pieces_;
   vector<Lane> lanes_;
};

}  // namespace game

#endif  // CPP_GAME_TRACK_H_
