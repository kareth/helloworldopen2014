#ifndef CPP_GAME_PIECE_H_
#define CPP_GAME_PIECE_H_

namespace game {

enum class PieceType {
  kBent,
  kStraight
};

class Piece {
 public:
  // Default constructor so that we can use it in vector.
  Piece() {}

  static Piece CreateStraight(double length, bool has_switch=false) {
    return Piece(length, has_switch);
  }

  static Piece CreateBent(double angle, double radius, bool has_switch=false) {
    return Piece(angle, radius, has_switch);
  }

  // Common fields.
  const PieceType& type() const { return type_; }
  const bool has_switch() const { return has_switch_; }

  // Only for PieceType::kStraight.
  const double length() const { return length_; }

  // Only for PieceType::kBent.
  const double angle() const { return angle_; }
  const double radius() const { return radius_; }

  bool operator==(const Piece& p) const {
    return type_ == p.type_ && length_ == p.length_ &&
           radius_ == p.radius_ && angle_ == p.angle_;
  }

 private:
  Piece(double length, bool has_switch)
    : type_(PieceType::kStraight), length_(length), has_switch_(has_switch) {
  }

  Piece(double angle, double radius, bool has_switch)
    : type_(PieceType::kBent), angle_(angle), radius_(radius),
      has_switch_(has_switch) {
  }

  PieceType type_;
  double length_ = 0;
  double radius_ = 0;
  double angle_ = 0;
  bool has_switch_ = false;
};

}  // namespace game

#endif  // CPP_GAME_PIECE_H_
