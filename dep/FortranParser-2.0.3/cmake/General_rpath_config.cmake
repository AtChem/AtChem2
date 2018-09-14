# this setting is taken from the CMAKE wiki at http://www.cmake.org/Wiki/CMake_RPATH_handling#Always_full_RPATH

MESSAGE( STATUS "Configuring rpath variables..." )

# use, i.e. don't skip the full RPATH for the build tree
SET ( CMAKE_SKIP_BUILD_RPATH  FALSE )

# when building, don't use the install RPATH already
# (but later on when installing)
SET ( CMAKE_BUILD_WITH_INSTALL_RPATH FALSE ) 

SET ( CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib" )

# *****************************************************************
# YOU NEED THIS ADDITION TO THE WIKI SCRIPT IN ORDER TO 
# CORRECTLY CONFIGURE RPATH ON MAC OS X 
SET ( CMAKE_INSTALL_NAME_DIR "${CMAKE_INSTALL_PREFIX}/lib" )
# *****************************************************************

# add the automatically determined parts of the RPATH
# which point to directories outside the build tree to the install RPATH
SET ( CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE )

# the RPATH to be used when installing, but only if it's not a system directory
LIST ( FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES "${CMAKE_INSTALL_PREFIX}/lib" isSystemDir )

IF ( "${isSystemDir}" STREQUAL "-1" )
   SET ( CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib" )
ENDIF ( "${isSystemDir}" STREQUAL "-1" )

# Flag telling that RPATH has been correctly configured
set ( RPATH_CONFIG true CACHE INTERNAL "RPATH_CONFIG" )

