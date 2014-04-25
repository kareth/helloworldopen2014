set(SCHEDULERS_SOURCES
  binary_throttle_scheduler.cpp
  greedy_turbo_scheduler.cpp
  shortest_path_switch_scheduler.cpp
  bulk_scheduler.cpp
)

set(SCHEDULERS_HEADERS
  throttle_scheduler.h
  binary_throttle_scheduler.h
  switch_scheduler.h
  turbo_scheduler.h
  greedy_turbo_scheduler.h
  shortest_path_switch_scheduler.h
  scheduler.h
  bulk_scheduler.h
)

add_library(schedulers ${SCHEDULERS_SOURCES})

add_style_check_target(schedulers "${SCHEDULERS_SOURCES}" HWO)
add_style_check_target(schedulers "${SCHEDULERS_HEADERS}" HWO)
