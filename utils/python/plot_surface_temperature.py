# -*- coding: utf-8 -*-
"""Plot observed and simulated zonal-mean surface temperature.

Author:             Andre Paul
Written:            2014-01-08
Last Change:        2018-11-15
        
Execution:          python plot_surface_temperature.py
Input file(s):      e.g., modern_climatology.dat,
                          energy_balance_1d_2.dat
                    
Output file(s):     
References:

"""

import numpy as np
import matplotlib.pyplot as plt

#-------------------------------------------------------------------------------
# Set directory names, file names and plot parameters
#-------------------------------------------------------------------------------

# Set directory and file names
experiment_name = "PD0"
model_filename  = experiment_name + "_energy_balance_1d.dat"
model_directory = "../../results/" + experiment_name + "/"
model_filepath  = model_directory + model_filename
data_filename   = "modern_climatology.dat"
data_directory  = "../../input/"
data_filepath   = data_directory + data_filename
plot_directory  = "../../plots/"
plot_filepath   = plot_directory + experiment_name + "_surface_temperature.pdf"

# Set figure title
figure_title = "Surface temperature (" + experiment_name + ")"

# Set figure width in cm
width_cm  = 22.86
height_cm =  10.0

#-------------------------------------------------------------------------------
# Read grid and data
#-------------------------------------------------------------------------------

# Read observed data from multi-column text file
data     = np.loadtxt(data_filepath, comments='%')
tobs_feb = data[:,  1]; # NH winter (February) surface temperature/degC
tobs_aug = data[:,  2]; # NH summer (August) surface temperature/degC
tobs_ann = data[:,  3]; # annual-mean surface temperature/degC

# Read simulated data from multi-column text file
data     = np.loadtxt(model_filepath, comments='%')
yt       = data[:,  0] # T grid cell latitude/degN
tsfc_feb = data[:, 13] # NH winter (February) surface temperature/degC
tsfc_aug = data[:, 14] # NH summer (August) surface temperature/degC
tsfc_ann = data[:, 15] # annual-mean surface temperature/degC

#-------------------------------------------------------------------------------
# Plot data
#-------------------------------------------------------------------------------

# Set figure size
inches_per_cm = 1.0/2.54
width_in  = width_cm*inches_per_cm  # width in inches
height_in = height_cm*inches_per_cm # height in inches

# Create figure and axes instances
fig = plt.figure(figsize=(width_in, height_in))
ax0 = fig.add_axes([0.0 , 0.0 , 1.0, 1.0]) # left, bottom, width, height
                                           # (range 0 to 1)
ax1 = fig.add_axes([0.10, 0.14, 0.4, 0.6])
ax2 = fig.add_axes([0.51, 0.14, 0.4, 0.6])

# Plot temperature curves
ax1.plot(yt, tsfc_feb, label="simulated")
ax1.plot(yt, tobs_feb, label="observed")
ax2.plot(yt, tsfc_aug, label="simulated")
ax2.plot(yt, tobs_aug, label="observed")

# Set limits of x and y axes
ax1.set_xlim([-90, 90])
ax1.set_ylim([-40, 40])
ax2.set_xlim([-90, 90])
ax2.set_ylim([-40, 40])

# Add title and annotate axes
ax0.text(0.5, 0.88, figure_title,
         horizontalalignment='center',
         fontsize=18)

# Add legends and annotate axes
ax1.legend(loc=4) # lower right corner
ax2.legend(loc=4) # lower right corner

# Annotate axes
ax1.set_title('NH winter (February)', fontsize=16)
ax1.set_xlabel('Latitude', fontsize=16)
ax1.set_ylabel(u"Temperature/\u00B0C", fontsize=16)
ax2.set_title('NH summer (August)', fontsize=16)
ax2.set_xlabel('Latitude', fontsize=16)
ax2.set_ylabel(u"Temperature/\u00B0C", fontsize=16)
ax2.yaxis.tick_right()
ax2.yaxis.set_label_position('right')

# Display result on screen
plt.show()

# Save plot to file
fig.savefig(plot_filepath, transparent=True)
