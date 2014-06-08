#ifndef CPP_UTILS_STOPWATCH_H_
#define CPP_UTILS_STOPWATCH_H_

#include <chrono>

namespace utils {
  typedef std::chrono::high_resolution_clock clock;
  typedef std::chrono::microseconds microseconds;


  class StopWatch
  {
   public:
    StopWatch() : start_(clock::now()) {}

    void reset() { start_ = clock::now(); }

    double elapsed() {
      //TODO(Wojtek): more precision
      return std::chrono::duration_cast<microseconds>(clock::now() - start_).count() / 1000.0;
    }

   private:
    clock::time_point start_;
  };

} // namespace utils

#endif 
