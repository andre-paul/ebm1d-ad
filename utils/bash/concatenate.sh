#!/bin/sh
#
# Written:        2010-08-22
# Last change:    2018-11-15
# Author:         Andre Paul (apaul@marum.de)
#
# Purpose:        Concatenate files for automatic differentiation

cat OptimizationParameters.f90 \
    Sizes.f90 \
    Constants.f90 \
    OrbParameters.f90 \
    RunParameters.f90 \
    Ebm1DGrids.f90 \
    Ebm1DParameters.f90 \
    Ebm1DVariables.f90 \
    Cost1DParameters.f90 \
    Cost1DVariables.f90 \
    InitialConditions.f90 \
    ReferenceData.f90 \
    Observations.f90 > ebm1d_data.f90

cat orbInitializeElements.f90 \
    orbComputeElements.f90 \
    calculateGlobalAverage.f90 \
    readInitialConditions.f90 \
    readReferenceData.f90 \
    orbTime2Longitude.f90 \
    orbDaily.f90 \
    readObservations.f90 \
    numbmod.f90 \
    initmod.f90 \
    initializeFixed.f90 \
    initializeVariables.f90 \
    readRunParameters.f90 \
    cost1DReadParameters.f90 \
    cost1DInitializeVariables.f90 \
    cost1DAccumulateMeans.f90 \
    ebm1DInitializeGrids.f90 \
    ebm1DReadParameters.f90 \
    ebm1DWriteParameters.f90 \
    ebm1DInitializeFixed.f90 \
    ebm1DInitializeVariables.f90 \
    ebm1DOutput.f90 \
    ebm1DCalculateIceAlbedo.f90 \
    ebm1DIntegrate.f90 \
    ebm1DWriteCosts.f90 \
    core.f90 \
    model.f90 > ebm1d_procedures.f90
