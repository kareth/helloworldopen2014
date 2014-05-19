cmake_minimum_required (VERSION 2.8.12.2)

project (HWO)

option(SET_WARNINGS "Compiler warning flags" OFF)
option(SET_DEBUG "Compiler debugging flags" OFF)
option(SET_CPPLINT "Style Check" OFF)

# Allow absolute paths
SET(BASEPATH "${CMAKE_SOURCE_DIR}")

SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
SET(gtest_SOURCE_DIR "${BASEPATH}/gtest")

# FIND_PACKAGE(Threads REQUIRED)
FIND_PACKAGE(Boost 1.54.0 COMPONENTS system filesystem REQUIRED)

enable_testing()

INCLUDE_DIRECTORIES("${BASEPATH}")
INCLUDE_DIRECTORIES("${BASEPATH}/jsoncons/src")
INCLUDE_DIRECTORIES(${gtest_SOURCE_DIR}/include ${gtest_SOURCE_DIR})
INCLUDE_DIRECTORIES(${Boost_INCLUDE_DIR})

include(EnableCxx11)
include(BuildType)
include(StyleCheck)

add_subdirectory(gtest)
add_subdirectory(gflags)
add_subdirectory(bots)
add_subdirectory(game)
add_subdirectory(utils)
add_subdirectory(physics)
add_subdirectory(schedulers)

add_library(common_flags common_flags.cpp)
target_link_libraries(common_flags gflags)

add_executable(bot main.cpp)
target_link_libraries(bot
  ${Boost_LIBRARIES}
  bots
  bots_basic
  bots_tomek
  bots_piotr
  bots_greedy
  bots_stepping
  bots_constant
  bots_kamikaze
  bots_wojtek
  common_flags
  game
  physics
  utils
  gflags
  schedulers
)
add_executable(throttle_tester throttle_tester.cpp)
target_link_libraries(throttle_tester
  ${Boost_LIBRARIES}
  bots
  bots_basic
  bots_tomek
  bots_piotr
  bots_greedy
  bots_stepping
  bots_constant
  bots_kamikaze
  bots_wojtek
  common_flags
  game
  physics
  utils
  gflags
  schedulers
)

add_style_check_target(bot main.cpp HWO)
