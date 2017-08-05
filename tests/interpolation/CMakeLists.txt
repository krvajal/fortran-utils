include_directories(${PROJECT_BINARY_DIR}/src)

project(interpolation)

set(LAPACK_LIBS
    lapack
    blas
    )

macro(ADDTEST name)
    add_executable(${name} ${name}.f90)
    target_link_libraries(${name} fortran_utils ${LAPACK_LIBS})
    add_test(${name} ${PROJECT_BINARY_DIR}/${name})
endmacro(ADDTEST)

ADDTEST(test_lagrangecoeff)

