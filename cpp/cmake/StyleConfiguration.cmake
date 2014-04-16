# This file contains style configuration - cpplint flags.

set(MAX_LINE_LENGTH 100)

set(STYLE_FILTER)
set(STYLE_FILTER ${STYLE_FILTER},-legal/copyright)
set(STYLE_FILTER ${STYLE_FILTER},-readability/streams)
set(STYLE_FILTER ${STYLE_FILTER},-readability/casting)
# set(STYLE_FILTER ${STYLE_FILTER},-build/include_order)
set(STYLE_FILTER ${STYLE_FILTER},-build/include_what_you_use)
