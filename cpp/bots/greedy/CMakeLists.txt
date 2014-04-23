set(BOTS_GREEDY_SOURCES
  bot.cpp
)

set(BOTS_GREEDY_HEADERS
  bot.h
)

set(GAME_TESTS
  bot_test.cpp
)

add_library(bots_greedy ${BOTS_GREEDY_SOURCES})

add_style_check_target(bots_greedy "${BOTS_GREEDY_SOURCES}" HWO)
add_style_check_target(bots_greedy "${BOTS_GREEDY_HEADERS}" HWO)

target_link_libraries(bots_greedy
  ${Boost_LIBRARIES}
  utils
  physics
  game)

add_executable(bots_greedy_tests ${GAME_TESTS})
target_link_libraries(bots_greedy_tests bots_greedy gtest gtest_main)
add_test(bots_greedy_tests bots_greedy_tests)
