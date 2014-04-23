#ifndef CPP_SCHEDULERS_STRATEGY_H_
#define CPP_SCHEDULERS_STRATEGY_H_

namespace schedulers {

enum class Strategy {
  kOptimizeRace,           // Optimizes race not to fall off
  kOptimizeCurrentLap,     // e.g Speeds up on the end even if it results in crash
  kOptimizeNextLap,        // Prepares best settings for next lap
  kBlock
};

}  // namespace schedulers

#endif  // CPP_SCHEDULERS_STRATEGY_H_
