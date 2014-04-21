set(BOTS_KARETH_SOURCES
  bot.cpp
)

set(BOTS_KARETH_HEADERS
  bot.h
)

add_library(bots_kareth ${BOTS_KARETH_SOURCES})

add_style_check_target(bots_kareth "${BOTS_KARETH_SOURCES}" HWO)
add_style_check_target(bots_kareth "${BOTS_KARETH_HEADERS}" HWO)

target_link_libraries(bots_kareth
  ${Boost_LIBRARIES}
  utils
  physics
  game)
