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

const int MagicThrottleScheduler::HORIZON = 40;
const int MagicThrottleScheduler::N = 60;

MagicThrottleScheduler::MagicThrottleScheduler(const game::Race& race,
    game::CarTracker& car_tracker, int time_limit)
  : race_(race), car_tracker_(car_tracker), best_schedule_(&car_tracker, HORIZON), time_limit_(time_limit)
{
}

void MagicThrottleScheduler::Schedule(const game::CarState& state) {
  best_schedule_.ShiftLeftFillSafe(state);
  best_schedule_.UpdateDistance(state);      // This is spurious, but just in case...

  if (!best_schedule_.IsSafe(state))         // Just in case  
    best_schedule_.Reset(state);
  
  Sched maxs(&car_tracker_, HORIZON);
  for (int i = 0;i < HORIZON; ++i)
      maxs.throttles[i] = 1.0;
  if (maxs.IsSafe(state)) {
    maxs.UpdateDistance(state);
    best_schedule_ = maxs;
  } else {
    ImproveByMagic(state, best_schedule_);
  }

  //TODO: Check if safe, make some local changes to make it safe

  Log(state);
  tick_ += 1;
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

vector<double> MakeMagic(int tick, int n, int horizon, const vector<double>& vm, const vector<double>& dm, double max_drift, double v0, double a_1, double a0, const vector<CarTracker::Curve>& curves, const vector<double>& initial_x) {

  std::ostringstream cmd;
   
  cmd << "python magic.py ";
  cmd << tick << " ";
  cmd << n << " " << horizon << " ";
  WriteDoubleVector(cmd, vm); cmd << " ";
  WriteDoubleVector(cmd, dm); cmd << " ";
  cmd << max_drift << " " << v0 << " " << a_1 << " " << a0 << " ";
  WriteCurves(cmd, curves); cmd << " ";
  WriteDoubleVector(cmd, initial_x);

  //TODO: Is this OK? Can I have any unexpected problems with this? NULL?
  //printf("%s\n", cmd.str().c_str());
  FILE* f = popen(cmd.str().c_str(), "r");
  vector<double> x;
  double v;
  while (fscanf(f, "%lf", &v) != EOF) {
    x.push_back(v);
  }
  pclose(f);
  //TODO: Check whether x has size of n. Fill with zeros otherwise.

  return x;
}

void MagicThrottleScheduler::ImproveByMagic(const game::CarState& state, Sched& schedule) {
  vector<double> dm = car_tracker_.GetDriftModel().GetModel();
  vector<double> vm = car_tracker_.GetVelocityModel().GetModel();
  double max_drift = car_tracker_.GetCrashModel().GetModel();

  //TODO: Appropriate distance (N*10 is not OK in case of turbo). Does adding more influence performance?
  vector<CarTracker::Curve> curves = car_tracker_.GetCurves(state, N*10); 

  Sched new_schedule(schedule);
  new_schedule.throttles = MakeMagic(tick_, N, HORIZON, vm, dm, max_drift, state.velocity(),
          state.previous_angle(), state.position().angle(), curves, schedule.throttles);
  new_schedule.UpdateDistance(state);

  // Just in case the magic makes something stupid
  if (new_schedule.distance > schedule.distance)
      schedule = new_schedule;
}

void MagicThrottleScheduler::Log(const game::CarState& state) {
  for (int i=0; i<20; ++i)
    printf("%.3f ", best_schedule_.throttles[i]);
  printf("\n");

  CarState next = state;
  for (int i=0; i<20; ++i) {
    next = car_tracker_.Predict(next, game::Command(best_schedule_.throttles[i]));
    printf("%.2f ", next.position().angle());
  }
  printf("\n");

  next = state;
  for (int i=0; i<20; ++i) {
    next = car_tracker_.Predict(next, game::Command(best_schedule_.throttles[i]));
    printf("%.3f ", next.velocity());
  }
  printf("\n");

/*  next = state;
  for (int i=0; i<20; ++i) {
    next = car_tracker_.Predict(next, game::Command(best_schedule_.throttles[i]));
    printf("%d(%.2f) ", next.position().piece(), next.position().piece_distance());
  }
  printf("\n");*/

  std::cout << "(" << state.position().piece() << ")" << " angle: " <<
    state.position().angle() << " velocity: " << state.velocity() <<
    std::endl;
}

}  // namespace schedulers
