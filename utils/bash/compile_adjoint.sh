#!/bin/sh
#
# Written:        2016-04-26
# Last change:    2018-11-15
# Author:         Andre Paul (apaul@marum.de)
#
# Purpose:        Compile adjoint code of Ebm1D

# Set name of executable file
EXEC=prgopti

# Determine host
host=`hostname -s`
echo "Compiling program '"$EXEC"' on host '"$host"'"

# Set directories and compiler options
SOURCEDIR=../src
WORKDIR=../run
COMPDIR=../build
echo "Source directory = "$SOURCEDIR
if [ "$host" = "endurance" ]; then
    COMPILER="gfortran"
    # COMPILER_OPTIONS="-fdefault-real-8 -g -Wall -pedantic -fbounds-check" # debugging
    COMPILER_OPTIONS="-fdefault-real-8 -march=native -O3 -ftree-vectorize -funroll-loops" # optimization
else
    COMPILER="gfortran"
    # COMPILER_OPTIONS="-fdefault-real-8 -g -Wall -pedantic -fbounds-check" # debugging
    # COMPILER_OPTIONS="-fdefault-real-8 -march=native -O3 -ftree-vectorize -funroll-loops" # optimization
    COMPILER_OPTIONS="-fdefault-real-8 -O3 -ftree-vectorize -funroll-loops" # optimization
fi
echo "Compiler         = "$COMPILER
echo "Compiler options = "$COMPILER_OPTIONS
    
# Change to compiling directory and clean it out
echo "==> changing to a compiling directory and cleaning it out ..."
cd $COMPDIR
pwd
rm *
cp -p $SOURCEDIR/*.f90 $COMPDIR
cp -p $SOURCEDIR/m1qn3.f $COMPDIR
cp -p $WORKDIR/concatenate.sh $COMPDIR

# Concatenate files
./concatenate.sh

# Compile program
echo "==> compiling ..."
# $COMPILER -ffree-form $COMPILER_OPTIONS -c ebm1d_data.f90 
# $COMPILER -ffree-form $COMPILER_OPTIONS -c ebm1d_procedures.f90
$COMPILER -ffree-form $COMPILER_OPTIONS -c ebm1d_data_ad.f90
$COMPILER -ffree-form $COMPILER_OPTIONS -c ebm1d_procedures_ad.f90
$COMPILER -P $COMPILER_OPTIONS -DLINUX -DREAL_BYTE=8 -DINTEGER_BYTE=4 -DUSE_NAMELIST -DDYNAMIC -c -o m1qn3.o m1qn3.f
$COMPILER -ffree-form $COMPILER_OPTIONS -c $EXEC.f90
echo "==> linking ..."
$COMPILER -o $EXEC *.o

# Copy executable to working directory
echo "==> copy program '"$EXEC"' to "$WORKDIR" ..."
cp $EXEC $WORKDIR

exit

