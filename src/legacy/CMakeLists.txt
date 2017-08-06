


set(common "-std=legacy -Wall -Wextra -Wimplicit-interface -fPIC  -fmax-errors=1")
set(CMAKE_Fortran_FLAGS_RELEASE "${common} -O3 -march=native -ffast-math -funroll-loops" )
set(CMAKE_Fortran_FLAGS_DEBUG   "${common} -g -fbounds-check -fcheck-array-temporaries -fbacktrace" )

add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/amos)
add_library(legacy ${LEGACY_SRC})
