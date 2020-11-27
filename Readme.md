
![](https://github.com/JeffIrwin/rubik/workflows/CI/badge.svg)

# rubik
Rubik's cube CLI game written in Fortran

### Clean
    rm *.mod *.o *.exe

### Compile
    make

### Run
    time ./cube.exe

The prefix `time` is unnecessary, but you might be curious how long it takes you to solve.

### Gameplay
Enter `h` for help during gameplay.

The faces of the cube are labelled right, up, left, down, front, and back.  The case sensitive letters `R`, `U`, `L`, `D`, `F`, and `B` will turn their respective face clockwise as if you were looking at that face.

The cube can be solved using only those six commands.  You may enter a series of moves on one line, separated by spaces (max 1000 characters).

To speed things up, you may append any of those six commands with an apostrophe (`'`) to turn the face counterclockwise, or a number two (`2`) to turn the face twice, e.g. `R'` or `R2`.

Lowercase moves turn a face and its adjacent middle slice, e.g. `r`, `u`, `l`, `d`, `f`, and `b`.

To rotate the whole cube about an axis, enter `x`, `y`, or `z`.  As above, you may also use e.g. `x'`, or `x2` to rotate by other degrees.  This is useful as you may want begin solving with the first layer on top, and then rotate the cube upside-down (`x2` or equivalently `z2`) to finish with the last layer on top.  During other steps, rotating about `y` is helpful to see an edge or corner piece of interest near the front view.

## Rubik's algorithms
This CLI game is almost impossible to play without a physical cube in front of you to aid in muscle memory.  For help, here are some algorithms that can be copy-pasted into gameplay.

### First layer
If you don't know these algorithms, you're gonna have a bad time.

### Middle layer
Middle layer step, moving an edge piece from the top to the middle:

Upper-left to front-right:

    R U' R' U' F' U F 

Upper-right to front-left:

    L' U L U F U' F' 

### Top layer
Top step, top edge orientation:

    F R U R' U' R U R' U' F' 

Top step, top edge permutation (repeat as necessary then follow by a final `U`):

    R U R' U R U2 R' 

Top step, top corner permutation:

    U R U' L' U R' U' L 

Top step, top corner orientation:

    R' D' R D 

