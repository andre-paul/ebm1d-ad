#!/bin/sh
#
# Written:        2011-07-19
# Last change:    2018-11-15
# Author:         Andre Paul (apaul@marum.de)
#
# Purpose:        Copy results to output directory
# Usage:          ./copy_results experiment_name

# Set experiment and directory names
experiment_name=$1
output_directory="../results/"$experiment_name
echo ">>> Saving model output"
echo "Experiment name:  " $experiment_name
echo "Output directory: " $output_directory

# Create target directory
mkdir -p $output_directory

# Copy files
cp -p Ebm1DParametersIn.nml   $output_directory/$experiment_name"_Ebm1DParametersIn.nml"
cp -p Ebm1DParametersOut.nml  $output_directory/$experiment_name"_Ebm1DParametersOut.nml"
cp -p RunParameters.nml       $output_directory/$experiment_name"_RunParameters.nml"
cp -p cost_function_terms.dat $output_directory/$experiment_name"_cost_function_terms.dat"
cp -p energy_balance_1d.dat   $output_directory/$experiment_name"_energy_balance_1d.dat"
cp -p energy_transport_1d.dat $output_directory/$experiment_name"_energy_transport_1d.dat"
cp -p printout.txt            $output_directory/$experiment_name"_printout.txt"
cp -p reference.dat           $output_directory/$experiment_name"_reference.dat"
cp -p restart.dat             $output_directory/$experiment_name"_restart.dat"
