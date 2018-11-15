#!/bin/sh
#
# Written:        2010-08-22
# Last change:    2018-11-15
# Author:         Andre Paul (apaul@marum.de)
#
# Purpose:        Generate the adjoint code of Ebm1D

# Set name of executable file
EXEC=Ebm1D

# Determine host
host=`hostname -s`
echo "Generating adjoint code for program '"$EXEC"' on host '"$host"'"

# Set directories and compiler options
SOURCEDIR=../src
WORKDIR=../run
COMPDIR=../build
echo "Source directory = "$SOURCEDIR
COMPILER=$WORKDIR"/taf"
COMPILER_OPTIONS="-toplevel model -reverse -input X -output FC"

echo "Compiler         = "$COMPILER
echo "Compiler options = "$COMPILER_OPTIONS

# Change to compiling directory and clean it out
echo "==> changing to a compiling directory and cleaning it out ..."
cd $COMPDIR
echo "Compiling directory:"
pwd
rm *
cp -p $SOURCEDIR/*.f90 $COMPDIR
cp -p $WORKDIR/concatenate.sh $COMPDIR

# Concatenate files
./concatenate.sh

# Compile program
echo "==> invoking TAF ..."
$COMPILER $COMPILER_OPTIONS ebm1d_data.f90 ebm1d_procedures.f90

# Copy newly generated source code
cp -p $COMPDIR/*ad.f90 $SOURCEDIR

exit

