set(BOTS_SOURCES
)

set(BOTS_HEADERS
  bot_api.h
)

#add_library(bots ${BOTS_SOURCES})

add_style_check_target(bots "${BOTS_SOURCES}" HWO)
add_style_check_target(bots "${BOTS_HEADERS}" HWO)

add_subdirectory(default)
