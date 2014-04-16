# Copyright (C) 2013 Daniel Scharrer
#
# This software is provided 'as-is', without any express or implied
# warranty.  In no event will the author(s) be held liable for any damages
# arising from the use of this software.
#
# Permission is granted to anyone to use this software for any purpose,
# including commercial applications, and to alter it and redistribute it
# freely, subject to the following restrictions:
#
# 1. The origin of this software must not be misrepresented; you must not
#    claim that you wrote the original software. If you use this software
#    in a product, an acknowledgment in the product documentation would be
#    appreciated but is not required.
# 2. Altered source versions must be plainly marked as such, and must not be
#    misrepresented as being the original software.
# 3. This notice may not be removed or altered from any source distribution.

# This is altered version.

find_package(PythonInterp)

# set cpplint flags
include(StyleConfiguration)

# Add a target that runs cpplint.py
#
# Parameters:
# - TARGET_NAME the name of the target to add
# - SOURCES_LIST a complete list of source and include files to check
function(add_style_check_target TARGET_NAME SOURCES_LIST PROJECT)

  if(NOT PYTHONINTERP_FOUND)
    return()
  endif()

  if(NOT SET_WARNINGS)
    return()
  endif()

  list(SORT SOURCES_LIST)
  list(REMOVE_DUPLICATES SOURCES_LIST)

  # add directory prefix
  SET(SOURCES_LIST_TMP)
  foreach(l ${SOURCES_LIST})
    list(APPEND SOURCES_LIST_TMP ${CMAKE_CURRENT_SOURCE_DIR}/${l} )
  endforeach()
  SET(SOURCES_LIST ${SOURCES_LIST_TMP})
  UNSET(SOURCES_LIST_TMP)

  # add custom command with PRE_BUILD option
  # builds only if there are no cpplint errors
  add_custom_command(TARGET ${TARGET_NAME}
    PRE_BUILD
    COMMAND "${CMAKE_COMMAND}" -E chdir
    "${CMAKE_SOURCE_DIR}"
    "${PYTHON_EXECUTABLE}"
    "${CMAKE_MODULE_PATH}/cpplint.py"
    "--filter=${STYLE_FILTER}"
    "--linelength=${MAX_LINE_LENGTH}"
    ${SOURCES_LIST}
    DEPENDS ${SOURCES_LIST}
    COMMENT "Checking code style."
    VERBATIM
    )

endfunction(add_style_check_target)
