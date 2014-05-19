set(BOTS_WOJTEK_SOURCES
  bot.cpp
)

set(BOTS_WOJTEK_HEADERS
  bot.h
)

add_library(bots_wojtek ${BOTS_WOJTEK_SOURCES})

add_style_check_target(bots_wojtek "${BOTS_WOJTEK_SOURCES}" HWO)
add_style_check_target(bots_wojtek "${BOTS_WOJTEK_HEADERS}" HWO)

target_link_libraries(bots_wojtek
  ${Boost_LIBRARIES}
  common_flags
  utils
  physics
  game)
