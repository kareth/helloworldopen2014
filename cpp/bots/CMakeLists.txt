set(BOTS_SOURCES
  raw_bot.cpp
)

set(BOTS_HEADERS
  bot_interface.h
  raw_bot.h
)

add_library(bots ${BOTS_SOURCES})

add_style_check_target(bots "${BOTS_SOURCES}" HWO)
add_style_check_target(bots "${BOTS_HEADERS}" HWO)

add_subdirectory(basic)
add_subdirectory(tomek)
add_subdirectory(piotr)
add_subdirectory(greedy)
add_subdirectory(stepping)
add_subdirectory(constant)

target_link_libraries(bots
  ${Boost_LIBRARIES}
  game)
