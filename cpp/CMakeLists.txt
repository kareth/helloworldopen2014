cmake_minimum_required (VERSION 2.8)

project (HWO)

option(SET_WARNINGS "Compiler warning flags" ON)
option(SET_DEBUG "Compiler debugging flags" OFF)

# Allow absolute paths
SET(BASEPATH "${CMAKE_SOURCE_DIR}")

SET (CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")

INCLUDE_DIRECTORIES("${BASEPATH}")
INCLUDE_DIRECTORIES("${BASEPATH}/jsoncons/src")

# enable_testing()
# include_directories(${gtest_SOURCE_DIR}/include ${gtest_SOURCE_DIR})

find_package(Boost 1.54.0 COMPONENTS system REQUIRED)
INCLUDE_DIRECTORIES(${Boost_INCLUDE_DIR})

include(EnableCxx11)
include(BuildType)
include(StyleCheck)

add_subdirectory(utils)
add_subdirectory(bots)
add_subdirectory(game)

add_executable(bot main.cpp)
target_link_libraries(bot
  ${Boost_LIBRARIES}
  utils
  game
  default_bot
)

add_style_check_target(bot main.cpp HWO)
