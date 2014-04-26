set(BOTS_TOMEK_SOURCES
  bot.cpp
)

set(BOTS_TOMEK_HEADERS
  bot.h
)

add_library(bots_tomek ${BOTS_TOMEK_SOURCES})

add_style_check_target(default_bot "${BOTS_TOMEK_SOURCES}" HWO)
add_style_check_target(default_bot "${BOTS_TOMEK_HEADERS}" HWO)

target_link_libraries(bots_tomek
  ${Boost_LIBRARIES}
  common_flags
  utils
  game)
