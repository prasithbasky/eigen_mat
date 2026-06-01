cmake_minimum_required(VERSION 3.10)
project(eigen_mat Fortran)

# Use gfortran if available
if(NOT CMAKE_Fortran_COMPILER)
    set(CMAKE_Fortran_COMPILER gfortran)
endif()

# Add compiler flags
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fdollar-ok -ffixed-line-length-none -fallow-argument-mismatch")

# Variables for BLAS and LAPACK subdirectories
set(BLASLIB blas)
set(LAPACKLIB lapack)
set(BUILD_DOUBLE ON)
set(BUILD_SINGLE OFF)
set(BUILD_COMPLEX OFF)
set(BUILD_COMPLEX16 OFF)
set(LAPACK_VERSION 3.12.1)
set(LAPACK_MAJOR_VERSION 3)

# Stub LAPACK macros
macro(lapack_install_library lib)
  # Do nothing for now
endmacro()

# Common include directories
include_directories(${CMAKE_SOURCE_DIR}/help)

# Add subdirectories for libraries
add_subdirectory(allg)
add_subdirectory(cha)
add_subdirectory(help)
add_subdirectory(matrix/engeln-reutter)
add_subdirectory(matrix/blas)
add_subdirectory(matrix/lapack)
add_subdirectory(matrix/arpack)

# Add test directory for executables
add_subdirectory(test)
