set(SCHEDULERS_SOURCES
  binary_throttle_scheduler.cpp
  wojtek_throttle_scheduler.cpp
  greedy_turbo_scheduler.cpp
  shortest_path_switch_scheduler.cpp
  bulk_scheduler.cpp
  learning_scheduler.cpp
  bump_scheduler.cpp
  schedule.cpp
  bb.cpp
)

set(SCHEDULERS_HEADERS
  throttle_scheduler.h
  binary_throttle_scheduler.h
  wojtek_throttle_scheduler.h
  switch_scheduler.h
  turbo_scheduler.h
  greedy_turbo_scheduler.h
  shortest_path_switch_scheduler.h
  scheduler.h
  bulk_scheduler.h
  learning_scheduler.h
  bump_scheduler.h
  schedule.h
  bb.h
)

add_library(schedulers ${SCHEDULERS_SOURCES})

add_style_check_target(schedulers "${SCHEDULERS_SOURCES}" HWO)
add_style_check_target(schedulers "${SCHEDULERS_HEADERS}" HWO)
