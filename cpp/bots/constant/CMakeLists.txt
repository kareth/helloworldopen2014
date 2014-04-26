set(BOTS_CONSTANT_SOURCES
  bot.cpp
)

set(BOTS_CONSTANT_HEADERS
  bot.h
)

add_library(bots_constant ${BOTS_CONSTANT_SOURCES})

add_style_check_target(bots_constant "${BOTS_CONSTANT_SOURCES}" HWO)
add_style_check_target(bots_constant "${BOTS_CONSTANT_HEADERS}" HWO)

target_link_libraries(bots_constant
  ${Boost_LIBRARIES}
  common_flags
  utils
  game)
