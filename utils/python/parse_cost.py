# -*- coding: utf-8 -*-
"""Parse standard output of Ebm1D to extract the cost function evolution.

This program extracts the time evolution of the cost function from the Ebm1D
standard output.

Author:             Andre Paul
Written:            2014-06-14
Last Change:        2018-11-15

Execution:          run parse_cost.py
Input file(s):      printout.txt
Output file:        cost.dat
References:         
"""
import sys
import time
import datetime
import numpy as np
import matplotlib.mlab as mlab

# If the script is invoked with a "-h" option (for "help"), then print the 
# docstring as its "usage" message
if len(sys.argv) == 2 and (sys.argv[1] == "-h"):
   print(__doc__)
   sys.exit(0)

#-------------------------------------------------------------------------------
# Set directory and file names
#-------------------------------------------------------------------------------

experiment_name  = "PD1"
input_directory  = "../../results/" + experiment_name + "/"
input_filename   = experiment_name + "_printout.txt"
input_filepath   = input_directory + input_filename
output_directory = "../../results/" + experiment_name + "/"
output_filename  = experiment_name + "_cost.dat"
output_filepath  = output_directory + output_filename

#-------------------------------------------------------------------------------
# Read data line by line, filter them and write them to a new file
#-------------------------------------------------------------------------------

fout = open(output_filepath, 'w')

with open(input_filepath, 'r') as fin:
    for line in fin:
        if line[1:12] == 'm1qn3: iter':
            iteration  = line[13:16]
            simulation = line[24:27]
            cost       = line[32:46]
            gradient   = line[54:66]
            print(iteration, simulation, cost, gradient)
            fout.write(iteration + '\t' + simulation + '\t' + cost + '\n')
                
fin.close()
fout.close()
