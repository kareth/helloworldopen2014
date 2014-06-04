#ifndef SRC_UTILS_DEADLINE_H_
#define SRC_UTILS_DEADLINE_H_

#include <memory>
#include <chrono>

namespace utils {

typedef std::chrono::duration<double> Seconds;
typedef std::chrono::time_point<std::chrono::system_clock, Seconds> TimePoint;

class Deadline {
 public:
  /** Creates a new deadline which will expire in durationToExpire.
   * Examples: 
   *   Deadline(std::chrono::seconds(3));
   *   Deadline(std::chrono::milliseconds(3000));
   */
  explicit Deadline(const Seconds& durationToExpire);

  /** Checks whether the deadline has expired */
  bool HasExpired() const;

  /** Returns a new deadline which will expire duration earlier than this deadline. */
  Deadline ShortenedBy(const Seconds& duration) const;

  /**
   * Returns a shortened version of the deadline. The new deadline will 
   * expire no later than in durationToExpire
   * It is equivalent to
   *   Deadline.Min(this, Deadline(durationToExpire))
   */
  Deadline TrimmedTo(const Seconds& durationToExpire) const;

  /** Returns deadline that will expire first */
  static const Deadline& Min(const Deadline& a, const Deadline& b) {
    return a.time_to_expire_ <= b.time_to_expire_ ? a : b;
  }

  /** Returns duration in which deadline with expire */
  Seconds GetDurationToExpire() const;

 private:
  const TimePoint time_to_expire_;
};

} // namespace

#endif
