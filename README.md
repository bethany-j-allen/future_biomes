# future_biomes
This repository contains the code associated with Allen BJ, Hill DJ, Burke AM, Clark M, Marchant R, Stringer LC, Williams DR, Lyon C (2024) Projected future climatic forcing on the global distribution of vegetation types, Philosophical Transactions of the Royal Society B, in press.

## Description of files

/data/biome_conversion.txt

Contains the biome to megabiome conversion table.

/R code/Read_nc.R

Code to read in netCDF files (containing BIOME4 outputs), clean, add area of cells, and convert to .csv

/R code/Read_footprint.R

Code to cut the HYDE anthrome footprints out of the BIOME4 outputs

/R code/Matrix_comparison.R

Code calculating the proportional area changing biomes over time (creates Figure 2a & 3a)

/R code/Biome_overlap.R

Code calculating the overlap of area attributed to each biome over time (creates Figure 2b & 3b)

/R code/Unoccupiable_cells.R

Code calculating the proportion of cells attributed to a biome which are not adjacent to that same biome in the previous time slice (creates Figure S5)

/R code/Biome_share.R

Code showing the change in total area for each biome over time (creates Figure S6 & S7)

/R code/Centroid_shift.R

Code calculating the latitudinal ranges and centroids for each biome over time (creates Figure S8)

/R code/Patchiness.R

Code calculating the number of patches of each biome over time (creates Figure S9)

