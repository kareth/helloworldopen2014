#ifndef CPP_GAME_PIECE_H_
#define CPP_GAME_PIECE_H_

namespace game {

enum PieceType {
  kBent,
  kStraight
};

class Piece {
 public:
 private:
   PieceType type_;
   double length_;
   double radius_;
   double angle_;
   bool switch_;
};

}  // namespace game

#endif  // CPP_GAME_PIECE_H_
