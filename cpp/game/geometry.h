#ifndef CPP_GAME_GEOMETRY_H_
#define CPP_GAME_GEOMETRY_H_

#include <algorithm>
#include <fstream>
#include <iostream>
#include <map>
#include <string>
#include <cmath>
#include <vector>

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

   double Distance(const Point& p) const { return sqrt((p.x() - x_) * (p.x() - x_) + (p.y() - y_) * (p.y() - y_)); }

   Point RotateOrigin(double angle) {
     double theta = angle * M_PI / 180.0;
     return Point(cos(theta) * x_ - sin(theta) * y_,
                  sin(theta) * x_ + cos(theta) * y_);
   }

 private:
   double x_, y_;
};

// Returns the radius of the circle that passes three points.
static double ArcRadius(const Point& a, const Point& b, const Point& c) {
  double x = a.Distance(b);
  double y = b.Distance(c);
  double z = c.Distance(a);

  double s = (x + y + z) / 2.0;

  return x * y * z / (4 * sqrt(s * (s - x) * (s - y) * (s - z)));
}

class QuadraticBezierCurve {
 public:
  QuadraticBezierCurve(const Point& s, const Point& c, const Point& e)
    : s_(s), c_(c), e_(e) { }

  static const int kSteps = 10000;

  double Length() {
    double length = 0;
    Point last = s_;
    // NOTE: This steps + 1 is intentional bug.
    for (double i = 1; i <= kSteps + 1; i++) {
      Point pt = PointAtPercent(double(i) / double(kSteps));
      length += last.Distance(pt);
      last = pt;
    }
    return length;
  }

  static QuadraticBezierCurve CreateForSwitch(
      double r1, double r2, double angle) {
    Point from = Point(-r1, 0);
    Point to = Point(-r2, 0).RotateOrigin(angle);

    Point center = Point(-(r1 + r2) / 2.0, 0).RotateOrigin(angle / 2.0);
    center = center * 2.0 - (from + to) * 0.5;

    return QuadraticBezierCurve(from, center, to);
  }

  std::vector<double> EstimateRadiuses() {
    std::vector<double> radiuses{0};

    std::vector<Point> points{s_};
    double length = Length();
    for (int i = 1; i <= kSteps + 1; i += 1) {
      Point pt = PointAtPercent(double(i) / double(kSteps));
      points.push_back(pt);
    }

    int percent = 0;
    double l = 0.0;
    for (int i = 1; i <= kSteps + 1; i += 1) {
      l += points[i].Distance(points[i-1]);
      if (static_cast<int>(100.0 * l / length) == percent + 1 && percent != 99) {
        percent++;
        double r = ArcRadius(points[i-1], points[i], points[i+1]);
        radiuses.push_back(r);
      }
    }
    return radiuses;
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
