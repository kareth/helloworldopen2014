#ifndef SCHEDULERS_BULK_SCHEDULER_H_
#define SCHEDULERS_BULK_SCHEDULER_H_

#include <string>
#include "game/command.h"
#include "game/car_tracker.h"
#include "schedulers/strategy.h"
#include "schedulers/scheduler.h"

#include "schedulers/greedy_turbo_scheduler.h"
#include "schedulers/shortest_path_switch_scheduler.h"
#include "schedulers/wojtek_throttle_scheduler.h"
#include "schedulers/bump_scheduler.h"

namespace schedulers {

class BulkScheduler : public Scheduler {
 public:
  BulkScheduler(const game::Race& race,
                game::RaceTracker& race_tracker,
                game::CarTracker& car_tracker,
                int time_limit);

  void Schedule(const game::CarState& state, int game_tick) override;

  void Overtake(const string& color) override;

  game::Command command() override { return command_; }

  void set_strategy(const Strategy& strategy) override;

  void IssuedCommand(const game::Command& command) override;

 private:
  ThrottleScheduler* CreateThrottleScheduler();
  SwitchScheduler* CreateSwitchScheduler();

  Strategy strategy_;
  game::Command command_;

  game::CarTracker& car_tracker_;
  game::RaceTracker& race_tracker_;
  const game::Race& race_;
  int time_limit_;

  std::unique_ptr<schedulers::ThrottleScheduler> throttle_scheduler_;
  std::unique_ptr<schedulers::SwitchScheduler> switch_scheduler_;
  std::unique_ptr<schedulers::TurboScheduler> turbo_scheduler_;
  std::unique_ptr<schedulers::BumpScheduler> bump_scheduler_;
};

}  // namespace schedulers

#endif  // SCHEDULERS_BULK_SCHEDULER_H_
