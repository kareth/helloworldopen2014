set(BOTS_BASIC_SOURCES
  bot.cpp
)

set(BOTS_BASIC_HEADERS
  bot.h
)

add_library(bots_basic ${BOTS_BASIC_SOURCES})

add_style_check_target(bots_basic "${BOTS_BASIC_SOURCES}" HWO)
add_style_check_target(bots_basic "${BOTS_BASIC_HEADERS}" HWO)

target_link_libraries(bots_basic
  ${Boost_LIBRARIES}
  common_flags
  utils
  game)
