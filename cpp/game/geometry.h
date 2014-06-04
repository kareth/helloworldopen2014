#ifndef CPP_GAME_GEOMETRY_H_
#define CPP_GAME_GEOMETRY_H_

#include <algorithm>
#include <fstream>
#include <iostream>
#include <map>
#include <string>

#define SQR(a) ((a)*(a))

namespace game {

class Point {
 public:
   Point() {}
   Point(double x, double y) : x_(x), y_(y) {}

   double x() const { return x_; }
   double y() const { return y_; }

   Point operator- (const Point& rhs) const { return Point(x_ - rhs.x(), y_ - rhs.y()); }
   Point operator+ (const Point& rhs) const { return Point(x_ + rhs.x(), y_ + rhs.y()); }
   Point operator* (double l) const { return Point(x_ * l, y_ * l); }

   double Distance(const Point& p) const { return sqrt(SQR(p.x() - x_) + SQR(p.y() - y_)); }

   Point RotateOrigin(double angle) {
     double theta = angle * M_PI / 180.0;
     return Point(cos(theta) * x_ - sin(theta) * y_,
                  sin(theta) * x_ + cos(theta) * y_);
   }

 private:
   double x_, y_;
};

class QuadraticBezierCurve {
 public:
  QuadraticBezierCurve(const Point& s, const Point& c, const Point& e)
    : s_(s), c_(c), e_(e) { }

  double Length(int steps) {
    double length = 0;
    Point last = s_;
    // NOTE: This steps + 1 is intentional bug.
    for (double i = 1; i <= steps + 1; i++) {
      Point pt = PointAtPercent(double(i) / double(steps));
      length += last.Distance(pt);
      last = pt;
    }
    return length;
  }

 private:
  Point PointAtPercent(double percent) {
    Point a = MidPoint(s_, c_, percent);
    Point b = MidPoint(c_, e_, percent);
    return MidPoint(a, b, percent);
  }

  Point MidPoint(const Point& a, const Point& b, double percent) {
    return a + (b - a) * percent;
  }

  Point s_, c_, e_;
};

}  // namespace game

#endif  // CPP_GAME_GEOMETRY_H_
