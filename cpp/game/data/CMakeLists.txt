configure_file(gameInit.json ${CMAKE_CURRENT_BINARY_DIR}/gameInit.json COPYONLY)
configure_file(gameInitGermany.json ${CMAKE_CURRENT_BINARY_DIR}/gameInitGermany.json COPYONLY)
configure_file(carPositions.json ${CMAKE_CURRENT_BINARY_DIR}/carPositions.json COPYONLY)
configure_file(greedyRun.json ${CMAKE_CURRENT_BINARY_DIR}/greedyRun.json COPYONLY)
configure_file(switchRun.json ${CMAKE_CURRENT_BINARY_DIR}/switchRun.json COPYONLY)
configure_file(turboRun.json ${CMAKE_CURRENT_BINARY_DIR}/turboRun.json COPYONLY)

add_subdirectory(maps)
