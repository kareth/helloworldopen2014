set(DEFAULT_BOT_SOURCES
  bot.cpp
)

set(DEFAULT_BOT_HEADERS
  bot.h
)

add_library(default_bot ${DEFAULT_BOT_SOURCES})

add_style_check_target(default_bot "${DEFAULT_BOT_SOURCES}" HWO)
add_style_check_target(default_bot "${DEFAULT_BOT_HEADERS}" HWO)
