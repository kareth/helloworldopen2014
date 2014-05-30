#include "schedulers/magic_throttle_scheduler.h"

#include <chrono>
#include <cmath>
#include <sstream>
#include <unistd.h>
#include <cstdio>
#include <streambuf>

#include "gflags/gflags.h"

namespace schedulers {

using game::CarState;
using game::CarTracker;

const int MagicThrottleScheduler::HORIZON = 50;
const int MagicThrottleScheduler::N = 100;

MagicThrottleScheduler::MagicThrottleScheduler(const game::Race& race,
    game::CarTracker& car_tracker, int time_limit)
  : race_(race), car_tracker_(car_tracker), best_schedule_(&car_tracker, HORIZON), time_limit_(time_limit) 
{
}

void MagicThrottleScheduler::Schedule(const game::CarState& state) {
  best_schedule_.ShiftLeftFillSafe(state);
  best_schedule_.UpdateDistance(state);      // Must do it, because throttle could have changed!
  //if (!best_schedule_.IsSafe(state))       // TODO: I'm not sure. Maybe we should make it safe?
  //  best_schedule_.Reset(state);
  
  ImproveByMagic(state, best_schedule_);

  //TODO: Check if safe, make some local changes to make it safe

  Log(state);
}

void WriteDoubleVector(std::ostringstream& os, const vector<double>& v) {
  os << "\"";
  for (int i=0; i<v.size(); ++i) {
    if (i != 0) os << " ";
    os << v[i];
  }
  os << "\"";
}

void WriteCurves(std::ostringstream& os, const vector<CarTracker::Curve>& v) {
  os << "\"";
  for (int i=0; i<v.size(); ++i) {
    if (i != 0) os << " ";
    os << v[i].direction << " " << v[i].rradius() << " " << v[i].length;
  }
  os << "\"";
}

vector<double> MakeMagic(int n, int horizon, const vector<double>& vm, const vector<double>& dm, double max_drift, double v0, double a_1, double a0, const vector<CarTracker::Curve>& curves, const vector<double>& initial_x) {

  std::ostringstream cmd;
   
  cmd << "python magic.py ";
  cmd << n << " " << horizon << " ";
  WriteDoubleVector(cmd, vm); cmd << " ";
  WriteDoubleVector(cmd, dm); cmd << " ";
  cmd << max_drift << " " << v0 << " " << a_1 << " " << a0 << " ";
  WriteCurves(cmd, curves); cmd << " ";
  WriteDoubleVector(cmd, initial_x);

  //TODO: Is this OK? Can I have any unexpected problems with this? NULL?
  FILE* f = popen(cmd.str().c_str(), "r");
  vector<double> x;
  double v;
  while (fscanf(f, "%lf", &v) != EOF) {
    x.push_back(v);
  }
  pclose(f);
  //TODO: Check whether x has size of n. What else?

  return x;
}

void MagicThrottleScheduler::ImproveByMagic(const game::CarState& state, Sched& schedule) {
  vector<double> dm = car_tracker_.GetDriftModel().GetModel();
  vector<double> vm = car_tracker_.GetVelocityModel().GetModel();
  double max_drift = car_tracker_.GetCrashModel().GetModel();

  //TODO: Appropriate distance
  vector<CarTracker::Curve> curves = car_tracker_.GetCurves(state, 1000); 

  schedule.throttles = MakeMagic(N, HORIZON, vm, dm, max_drift, state.velocity(),
          state.previous_angle(), state.position().angle(), curves, schedule.throttles);
  schedule.UpdateDistance(state);
}

void MagicThrottleScheduler::Log(const game::CarState& state) {
  for (int i=0; i<HORIZON; ++i)
    printf("%.1f ", best_schedule_.throttles[i]);
  printf("\n");

  std::cout << "(" << state.position().piece() << ")" << " angle: " <<
    state.position().angle() << " velocity: " << state.velocity() <<
    std::endl;
}

}  // namespace schedulers
