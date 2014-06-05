#include "utils/deadline.h"

#include <stdexcept>
#include <algorithm>

namespace utils {

using std::chrono::system_clock;

Deadline::Deadline(const Seconds& durationToExpire) :
  time_to_expire_(system_clock::now() + durationToExpire) {
}

bool Deadline::HasExpired() const {
  return system_clock::now() >= time_to_expire_;
}

Deadline Deadline::ShortenedBy(const Seconds& duration) const {
  return Deadline(GetDurationToExpire() - duration);
}

Deadline Deadline::TrimmedTo(const Seconds& duration) const {
  return Deadline(std::min(GetDurationToExpire(), duration));
}

Seconds Deadline::GetDurationToExpire() const {
  Seconds dt = time_to_expire_ - system_clock::now();
  return std::max(dt, Seconds::zero());
}

}
