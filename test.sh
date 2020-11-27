#!/bin/bash

inputs=./tests/test-*.inp

frames=( )

outputext=out

# Extra arguments for each test.  "-s" seeds the random cube scrambler.
args=( "-s 0"
     )

exebase=cube
outdir=.
expectedoutdir=./expected/
use_stdout="true"
use_localoutdir="true"

#===============================================================================

source ./submodules/bat/test.sh

