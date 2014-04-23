set(GAME_SOURCES
  car.cpp
  car_tracker.cpp
  command.cpp
  gauss.cpp
  position.cpp
  race.cpp
  result.cpp
  simplex.cpp
  track.cpp
  turbo.cpp
)

set(GAME_HEADERS
  car.h
  car_predictor.h
  car_tracker.h
  command.h
  crash_model.h
  drift_model.h
  error_tracker.h
  gauss.h
  lane.h
  position.h
  race.h
  result.h
  simplex.h
  track.h
  velocity_model.h
  turbo.h
)

set(GAME_TESTS
  car_tracker_test.cpp
  gauss_test.cpp
  position_test.cpp
  race_test.cpp
  simplex_test.cpp
  track_test.cpp
)

add_library(game ${GAME_SOURCES})

add_style_check_target(game "${GAME_SOURCES}" HWO)
add_style_check_target(game "${GAME_HEADERS}" HWO)

add_subdirectory(data)

add_executable(game_tests ${GAME_TESTS})
target_link_libraries(game_tests game gtest gtest_main)
add_test(game_tests game_tests)
