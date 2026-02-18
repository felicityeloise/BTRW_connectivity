# Written by Felicity Charles
# 02/10/2025
# Caveat emptor

## This script produces resistance surfaces from the environmental data
# Probably need some sort of data on water - is NDVI sufficient for this to show the BTRW cannot move through bodies of water but has to go around - maybe BVGs will give us some info too


# gdistance package works off conductance matrices rather than resistance 

# RestistanceGA provides functions to prepare data and execute a number of functions to optimize continuous and categorical resistance surfaces using CIRCUITSCAPE, CIRCUITSCAPE written in Julia and Genetic Algorithms within R. You must have CIRCUITSCAPE (4.0-Beta or higher) or Julia installed to run these functions. Use of this package to run CIRCUITSCAPE is limited to Windows machines due its use of the Circuitscape .exe file. However, Julia can be installed on any operating system, making this a more versatile option.The continued development and support of functions is primarily occurring with 'gdistsance' and Julia implementations.


# Can use habitat suitability models to determine landscape resistance with lower suitability = higher resistance but may not be optimal as species will still traverse sub-optimal habitat. We can also use species occurrences to generate resistance surfaces by building resource selection metrics and converting this to resistance. 





# Use ResistanceGA, radish, gdistance, or movecost for constructing resistance surfaces, with the first two completing automised optimisation. Then we can use landscapemetrics in R which is just like FRAGSTATS to calculate connectivity indices
# Radish should only be used as a tool for restistance surface optimisation
# radish currently fits a loglinear relationship between spatial covariates and conductance/resistance. In contrast, ResistanceGA allows for the fitting of more complex non-linear relationships


# Investigate buffers at the edges of map, I have expanded the aoi beyond just where we had presence data but look at Koen EL, Garroway CJ, Wilson PJ, Bowman J (2010) The effect of map boundary on estimates of landscape resistance to animal movement. PLoS ONE 5:1–8


# We want to use a matrix selection function with resistance distance as the ecological distance measure?