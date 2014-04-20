set(GFLAGS_SOURCES
  gflags.cc
  gflags_completions.cc
  gflags_reporting.cc
)

add_library(gflags ${GFLAGS_SOURCES})
