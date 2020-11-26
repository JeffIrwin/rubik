
# fortran compiler
FC = gfortran

# compiler flags
CFLAGS = -c -I. -I..
#CFLAGS = -c -I. -I/fortran -fbounds-check

# linker flags
# mkl is not installed on this system
#LFLAGS = -mkl

# included modules must appear before calling programs
SOURCES = randmod.f textmod.f cubemod.f cube.f
OBJECTS = $(SOURCES:.f=.o)
EXECUTABLE = cube

all: $(SOURCES) $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS)
	$(FC) $(LFLAGS) $(OBJECTS) -o $@

.f.o:
	$(FC) $(CFLAGS) $< -o $@

