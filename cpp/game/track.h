#ifndef CPP_GAME_TRACK_H_
#define CPP_GAME_TRACK_H_

namespace game {

class Track {
 public:
   // TODO(tomek) iterator maybe?
 private:
   string id_;
   string name_;
   std::vector<Piece> pieces_;
   std::vector<Lane> lanes_;
};

}  // namespace game

#endif  // CPP_GAME_TRACK_H_
