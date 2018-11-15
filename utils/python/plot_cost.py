# -*- coding: utf-8 -*-
"""Plot the time evolution of the Ebm1D cost function.

Author:             Andre Paul
Written:            2014-01-08
Last Change:        2018-11-15
        
Execution:          run plot_cost.py
Input file(s):      cost.dat
                    
Output file(s):     
References:

"""

import numpy as np
import matplotlib.pyplot as plt

#-------------------------------------------------------------------------------
# Set directory name, file name and plot parameters
#-------------------------------------------------------------------------------

# Set directories and filenames
experiment_name  = "PD1"
input_directory  = "../../results/" + experiment_name + "/"
input_filename   = experiment_name + "_cost.dat"
input_filepath   = input_directory + input_filename
output_directory = "../../plots/"
output_filename  = experiment_name + "_cost.pdf"
output_filepath  = output_directory + output_filename


# Set figure width in cm
width_cm  = 22.86
height_cm =  10.0

#-------------------------------------------------------------------------------
# Read data
#-------------------------------------------------------------------------------

data       = np.loadtxt(input_filepath, comments='%')
iteration  = data[:, 0]
simulation = data[:, 1] 
cost       = data[:, 2]
# gradient   = data[:, 3]

#-------------------------------------------------------------------------------
# Plot data
#-------------------------------------------------------------------------------

# Set figure size
inches_per_cm = 1.0/2.54
width_in  = width_cm*inches_per_cm  # width in inches
height_in = height_cm*inches_per_cm # height in inches

# Create figure and axes instances
fig = plt.figure(figsize=(width_in, height_in))
ax  = fig.add_axes([0.1, 0.13, 0.8, 0.8]) # left, bottom, width, height
                                          # (range 0 to 1)

# Plot data
ax.plot(iteration, cost, marker='o', markersize=4)

# Add title
ax.set_title('Cost function', fontsize=16)

# Annotate axes
ax.set_xlabel('Iteration number', fontsize=16)
ax.set_ylabel(u"Cost function value", fontsize=16)

# Display result on screen
plt.show()

# Save plot to file
fig.savefig(output_filepath, transparent=True)
