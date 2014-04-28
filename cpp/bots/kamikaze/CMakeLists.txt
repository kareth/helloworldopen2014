set(BOTS_KAMIKAZE_SOURCES
  bot.cpp
)

set(BOTS_KAMIKAZE_HEADERS
  bot.h
)

add_library(bots_kamikaze ${BOTS_KAMIKAZE_SOURCES})

add_style_check_target(bots_kamikaze "${BOTS_KAMIKAZE_SOURCES}" HWO)
add_style_check_target(bots_kamikaze "${BOTS_KAMIKAZE_HEADERS}" HWO)

target_link_libraries(bots_kamikaze
  ${Boost_LIBRARIES}
  common_flags
  utils
  physics
  game)
