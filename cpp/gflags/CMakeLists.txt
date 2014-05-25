set(GFLAGS_SOURCES
  gflags.cc
  gflags_completions.cc
  gflags_reporting.cc
)

add_library(gflags ${GFLAGS_SOURCES})

target_link_libraries(gflags
  ${CMAKE_THREAD_LIBS_INIT}
)
