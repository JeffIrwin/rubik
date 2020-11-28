
cmake_minimum_required(VERSION 3.7)

set(PROJECT rubik)

set(CMAKE_Fortran_FLAGS "-Wall -Wextra -fopenmp -Wno-tabs")
set(CMAKE_Fortran_FLAGS_DEBUG "-pedantic -fbounds-check -fbacktrace")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3")

set(CMAKE_Fortran_COMPILER "gfortran")

project(${PROJECT} Fortran)

set(SRCDIR "src")

add_executable(${PROJECT}
	${SRCDIR}/randmod.f90
	${SRCDIR}/textmod.f90
	${SRCDIR}/rubik.f90
	${SRCDIR}/main.f90
	)

