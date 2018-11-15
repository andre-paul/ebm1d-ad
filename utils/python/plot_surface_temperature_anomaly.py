# -*- coding: utf-8 -*-
"""Plot the observed and simulated zonal-mean surface temperature anomalies.

Author:             Andre Paul
Written:            2014-01-08
Last Change:        2018-11-15
        
Execution:          python plot_surface_temperature_anomalies.py
Input file(s):      e.g., modern_climatology.dat,
                          energy_balance_1d_2.dat,
                          PD1_reference.dat
                    
Output file(s):     
References:

"""

import numpy as np
import matplotlib.pyplot as plt

#-------------------------------------------------------------------------------
# Set directory names, file names and plot parameters
#-------------------------------------------------------------------------------

# Set directory and file names
experiment_name = "LGM2"
model_filename  = experiment_name + "_energy_balance_1d.dat"
model_directory = "../results/" + experiment_name + "/"
model_filepath  = model_directory + model_filename
data_filename   = "glamap_c_lgm_sst_anomaly_r36x18_zm.dat"
data_directory  = "../input/"
data_filepath   = data_directory + data_filename
reference_filename  = "PD1_reference.dat"
reference_directory = "../results/PD1/"
reference_filepath  = reference_directory + reference_filename
plot_directory   = "../plots/"
plot_filepath    = plot_directory + experiment_name + "_surface_temperature_anomaly.pdf"

# Set figure width in cm
width_cm  = 22.86
height_cm =  10.0

# Set figure title
figure_title = "Surface temperature anomaly (" + experiment_name + " vs. observed)"

#-------------------------------------------------------------------------------
# Read grid and data
#-------------------------------------------------------------------------------

# Read observed data from multi-column text file
data          = np.loadtxt(data_filepath, comments='%')
data[data==-99.99]=np.nan
tobs_feb_anom = data[:,  1]; # NH winter (February) surface temperature/degC
tobs_aug_anom = data[:,  2]; # NH summer (August) surface temperature/degC
tobs_ann_anom = data[:,  3]; # annual-mean surface temperature/degC

# Read simulated data from multi-column text file
data     = np.loadtxt(model_filepath, comments='%')
yt       = data[:,  0] # T grid cell latitude/degN
tsfc_feb = data[:, 13] # NH winter (February) surface temperature/degC
tsfc_aug = data[:, 14] # NH summer (August) surface temperature/degC
tsfc_ann = data[:, 15] # annual-mean surface temperature/degC


# Read reference data from multi-column text file
data     = np.loadtxt(reference_filepath, comments='%')
tref_feb = data[:,  1] # NH winter (February) surface temperature/degC
tref_aug = data[:,  2] # NH summer (August) surface temperature/degC
tref_ann = data[:,  3] # annual-mean surface temperature/degC

# Calculate differences
tsfc_feb_anom = tsfc_feb - tref_feb
tsfc_aug_anom = tsfc_aug - tref_aug

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
ax0.set_axis_off()
ax1 = fig.add_axes([0.10, 0.14, 0.4, 0.6]) 
ax2 = fig.add_axes([0.51, 0.14, 0.4, 0.6])

# Plot temperature curves
ax1.plot(yt, tsfc_feb_anom, label='simulated')
ax1.plot(yt, tobs_feb_anom, label='reconstructed')
ax2.plot(yt, tsfc_aug_anom, label='simulated')
ax2.plot(yt, tobs_aug_anom, label='reconstructed')

# Set limits of x and y axes
ax1.set_xlim([-90, 90])
ax1.set_ylim([-6, 2])
ax2.set_xlim([-90, 90])
ax2.set_ylim([-6, 2])

# Add title
ax0.text(0.5, 0.88, figure_title,
         horizontalalignment='center',
         fontsize=18)
         
# Add legends
ax1.legend(loc=3) # lower left corner
ax2.legend(loc=3) # lower left corner

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
