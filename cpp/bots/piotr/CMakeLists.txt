set(BOTS_PIOTR_SOURCES
  bot.cpp
)

set(BOTS_PIOTR_HEADERS
  bot.h
)

add_library(bots_piotr ${BOTS_PIOTR_SOURCES})

add_style_check_target(bots_piotr "${BOTS_PIOTR_SOURCES}" HWO)
add_style_check_target(bots_piotr "${BOTS_PIOTR_HEADERS}" HWO)

target_link_libraries(bots_piotr
  ${Boost_LIBRARIES}
  utils
  game)
