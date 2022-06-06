#Bethany Allen   6th June 2022
#Code to calculate the number of grid cells which have changed between slices

biomes <- read.csv("data/cleaned/RCP6.csv")

length(which(biomes$X2000.2019 != biomes$X2020.2039))

