set(BOTS_SWITCH_OPTIMIZER_SOURCES
  bot.cpp
)

set(BOTS_SWITCH_OPTIMIZER_HEADERS
  bot.h
)

add_library(bots_switch_optimizer ${BOTS_SWITCH_OPTIMIZER_SOURCES})

add_style_check_target(bots_switch_optimizer "${BOTS_SWITCH_OPTIMIZER_SOURCES}" HWO)
add_style_check_target(bots_switch_optimizer "${BOTS_SWITCH_OPTIMIZER_HEADERS}" HWO)

target_link_libraries(bots_switch_optimizer
  ${Boost_LIBRARIES}
  common_flags
  utils
  physics
  game)
