set(SCHEDULERS_SOURCES
  binary_throttle_scheduler.cpp
)

set(SCHEDULERS_HEADERS
  throttle_scheduler.h
  binary_throttle_scheduler.h
  switch_scheduler.h
  turbo_scheduler.h
)

add_library(schedulers ${SCHEDULERS_SOURCES})

add_style_check_target(schedulers "${SCHEDULERS_SOURCES}" HWO)
add_style_check_target(schedulers "${SCHEDULERS_HEADERS}" HWO)
