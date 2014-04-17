set(GAME_SOURCES
  car.cpp
  position.cpp
  command.cpp
  race.cpp
  track.cpp
)

set(GAME_HEADERS
  car.h
  command.h
  lane.h
  position.h
  race.h
  track.h
)

set(GAME_TESTS
  race_test.cpp
  track_test.cpp
  position_test.cpp
)

add_library(game ${GAME_SOURCES})

add_style_check_target(game "${GAME_SOURCES}" HWO)
add_style_check_target(game "${GAME_HEADERS}" HWO)

add_executable(game_tests ${GAME_TESTS})
target_link_libraries(game_tests game gtest gtest_main)
add_test(game_tests game_tests)
