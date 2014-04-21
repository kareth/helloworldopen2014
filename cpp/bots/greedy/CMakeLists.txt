set(BOTS_GREEDY_SOURCES
  bot.cpp
)

set(BOTS_GREEDY_HEADERS
  bot.h
)

add_library(bots_greedy ${BOTS_GREEDY_SOURCES})

add_style_check_target(bots_greedy "${BOTS_GREEDY_SOURCES}" HWO)
add_style_check_target(bots_greedy "${BOTS_GREEDY_HEADERS}" HWO)

target_link_libraries(bots_greedy
  ${Boost_LIBRARIES}
  utils
  physics
  game)
