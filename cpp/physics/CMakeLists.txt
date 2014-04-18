set(PHYSICS_SOURCES
  speed_tracker.cpp
)

set(PHYSICS_HEADERS
  speed_tracker.h
)

add_library(physics ${PHYSICS_SOURCES})

add_style_check_target(physics "${PHYSICS_SOURCES}" HWO)
add_style_check_target(physics "${PHYSICS_HEADERS}" HWO)

target_link_libraries(physics game)
