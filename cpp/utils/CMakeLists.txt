set(UTILS_SOURCES
  connection.cpp
  protocol.cpp
  game_visualizer.cpp
)

set(UTILS_HEADERS
  protocol.h
  connection.h
  game_visualizer.h
)

add_library(utils ${UTILS_SOURCES})

add_style_check_target(utils "${UTILS_SOURCES}" HWO)
add_style_check_target(utils "${UTILS_HEADERS}" HWO)
