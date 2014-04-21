set(BOTS_STEPPING_SOURCES
  bot.cpp
)

set(BOTS_STEPPING_HEADERS
  bot.h
)

add_library(bots_stepping ${BOTS_STEPPING_SOURCES})

add_style_check_target(bots_stepping "${BOTS_STEPPING_SOURCES}" HWO)
add_style_check_target(bots_stepping "${BOTS_STEPPING_HEADERS}" HWO)

target_link_libraries(bots_stepping
  ${Boost_LIBRARIES}
  utils
  physics
  game)
