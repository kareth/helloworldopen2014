set(GAME_SOURCES
  approximation.cpp
  car.cpp
  car_tracker.cpp
  command.cpp
  gauss.cpp
  position.cpp
  race.cpp
  radius_model.cpp
  result.cpp
  simplex.cpp
  track.cpp
  turbo.cpp
  lane_length_model.cpp
)

set(GAME_HEADERS
  approximation.h
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
  radius_model.h
  result.h
  simplex.h
  track.h
  velocity_model.h
  lane_length_model.h
  turbo.h
)

set(GAME_TESTS
  approximation_test.cpp
  car_tracker_test.cpp
  drift_model_test.cpp
  gauss_test.cpp
  position_test.cpp
  race_test.cpp
  simplex_test.cpp
  track_test.cpp
  lane_length_model_test.cpp
  radius_model_test.cpp
)

add_library(game ${GAME_SOURCES})

add_style_check_target(game "${GAME_SOURCES}" HWO)
add_style_check_target(game "${GAME_HEADERS}" HWO)

add_subdirectory(data)

add_executable(game_tests ${GAME_TESTS})
target_link_libraries(game_tests game gtest gtest_main)
add_test(game_tests game_tests)
