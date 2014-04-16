set(GAME_SOURCES
  track.cpp
)

set(GAME_HEADERS
  car.h
  lane.h
  position.h
  race.h
  track.h
)

add_library(game ${GAME_SOURCES})

add_style_check_target(game "${GAME_SOURCES}" HWO)
add_style_check_target(game "${GAME_HEADERS}" HWO)
