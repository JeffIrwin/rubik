
cmake_minimum_required(VERSION 3.7)

set(PROJECT cube)

set(CMAKE_Fortran_FLAGS "-Wall -Wextra -fopenmp -Wno-tabs")
set(CMAKE_Fortran_FLAGS_DEBUG "-pedantic -fbounds-check -fbacktrace")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3")

set(CMAKE_Fortran_COMPILER "gfortran")

project(${PROJECT} Fortran)

add_executable(${PROJECT} randmod.f textmod.f cubemod.f cube.f)

