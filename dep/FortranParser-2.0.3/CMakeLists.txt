# CMake project file for mt_stream
cmake_minimum_required ( VERSION 2.6 )

# Allows to set project version
cmake_policy(SET CMP0048 NEW)
project (FortranParser VERSION 2.0.3)

# Which languages do we use
enable_language ( Fortran )

# Set the CMAKE_MODULE_PATH
LIST (APPEND CMAKE_MODULE_PATH $ENV{CMAKE_MACROS})
LIST ( APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" )

set(ENABLE_TESTING ON CACHE INTERNAL "flag to enable unit tests")

# Check that when using GNU Fortran the free form flag is passed
if ( NOT Fortran_FLAGS_ARE_SET )
  if ( CMAKE_Fortran_COMPILER_ID STREQUAL GNU )
    include( GNU_Fortran_Flags )
  elseif ( CMAKE_Fortran_COMPILER_ID STREQUAL Intel )
    include( Intel_Fortran_Flags )
  endif ( CMAKE_Fortran_COMPILER_ID STREQUAL GNU )
endif ( NOT Fortran_FLAGS_ARE_SET )

if ( NOT RPATH_CONFIG )
  include ( General_rpath_config ) 
endif ( NOT RPATH_CONFIG )


# Set library name
set ( LIBRARY_NAME fortranparser )

# Whether you build a static or shared library
set(LIBRARY_TYPE SHARED)
if (CMAKE_LIBRARY_TYPE)
  set(LIBRARY_TYPE ${CMAKE_LIBRARY_TYPE})
endif (CMAKE_LIBRARY_TYPE)

# Library installation directory
set (CMAKE_LIBDIR ${CMAKE_INSTALL_PREFIX}/lib)

# Header files installation directory
set (CMAKE_INCLUDEDIR ${CMAKE_INSTALL_PREFIX}/include/${LIBRARY_NAME})

# Binary files installation directory
set (CMAKE_BINDIR ${CMAKE_INSTALL_PREFIX}/bin)

# Set installation directory for *mod files
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod_files)

# Source files used to build the library
FILE(GLOB SRC_FILES RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/src/*.f90 ${CMAKE_CURRENT_SOURCE_DIR}/src/*.F90)

# Command to build the library
add_library(
  ${LIBRARY_NAME}
  ${LIBRARY_TYPE} 
  ${SRC_FILES}
)

# install library
install(
  TARGETS ${LIBRARY_NAME} 
  ARCHIVE DESTINATION ${CMAKE_LIBDIR}
  LIBRARY DESTINATION ${CMAKE_LIBDIR}
)

# install header (*mod) files
install(
  DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/ 
  DESTINATION ${CMAKE_INCLUDEDIR}
)

# configure a number version file to pass some of the CMake settings
# to the source code
configure_file(
  $ENV{CMAKE_MACROS}/pkg-config.pc.cmake
  ${CMAKE_CURRENT_SOURCE_DIR}/${LIBRARY_NAME}.pc
  @ONLY
)

# install configuration file
install(
  FILES ${CMAKE_CURRENT_SOURCE_DIR}/${LIBRARY_NAME}.pc
  DESTINATION ${CMAKE_LIBDIR}/pkgconfig
  )

set_target_properties(
  ${LIBRARY_NAME}
  PROPERTIES COMPILE_FLAGS "${Fortran_FLAGS} ${CMAKE_Fortran_COMPILER_FLAGS}"
  )

# set variables which will be sued by a calling CMakeLists.txt file
set ( FORTRANPARSER_INCLUDE_DIR "${CMAKE_INCLUDEDIR}" CACHE INTERNAL "FORTRANPARSER include directory" )
set ( FORTRANPARSER_LIBRARIES_DIR "${CMAKE_LIBDIR}" CACHE INTERNAL "FORTRANPARSER library directory" )
set ( FORTRANPARSER_LIBRARIES "${LIBRARY_NAME}" CACHE INTERNAL "FORTRANPARSER library" )

# *******************************************
# ************* UNINSTALL THE LIBRARY *******
# *******************************************
configure_file(
    "$ENV{CMAKE_MACROS}/cmake_uninstall.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/cmake/cmake_uninstall.cmake"
    IMMEDIATE @ONLY)

add_custom_target(uninstall
    COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/cmake/cmake_uninstall.cmake)

# **************************************************
# ************* EXTRACT THE PACKAGE VERSION FROM THE GIT TAG
# **************************************************
configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/cmake/${LIBRARY_NAME}_meta_info.cmake.in"
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/${LIBRARY_NAME}_meta_info.cmake"
)

add_custom_command(TARGET ${LIBRARY_NAME} PRE_BUILD COMMAND cmake -P ${CMAKE_CURRENT_BINARY_DIR}/cmake/${LIBRARY_NAME}_meta_info.cmake)

# Testing routine
if (ENABLE_TESTING)
  enable_testing( )

  add_executable( test0 "${CMAKE_CURRENT_SOURCE_DIR}/src/tests/testfp.f90" )
  target_link_libraries( test0 ${LIBRARY_NAME} )
  add_test( test0 test0 )

  add_executable( test1 "${CMAKE_CURRENT_SOURCE_DIR}/src/tests/testfp1.f90" )
  target_link_libraries( test1 ${LIBRARY_NAME} )
  add_test( test1 test1 )

  add_executable( test2 "${CMAKE_CURRENT_SOURCE_DIR}/src/tests/testfp2.f90" )
  target_link_libraries( test2 ${LIBRARY_NAME} )
  add_test( test2 test2 )

  add_executable( test3 "${CMAKE_CURRENT_SOURCE_DIR}/src/tests/testfp3.f90" )
  target_link_libraries( test3 ${LIBRARY_NAME} )
  add_test( test3 test3 )

  add_executable( test4 "${CMAKE_CURRENT_SOURCE_DIR}/src/tests/testfp4.f90" )
  target_link_libraries( test4 ${LIBRARY_NAME} )
  add_test( test4 test4 )

  add_executable( test5 "${CMAKE_CURRENT_SOURCE_DIR}/src/tests/testfp5.f90" )
  target_link_libraries( test5 ${LIBRARY_NAME} )
  add_test( test5 test5 )
endif(ENABLE_TESTING)



