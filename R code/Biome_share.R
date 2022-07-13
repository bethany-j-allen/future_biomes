#Bethany Allen   7th July 2022
#Code to calculate the proportion of grid cells attributed to each biome over time

library(tidyverse)
library(rlang)

#Read in biome IDs
biomes <- read.csv("data/cleaned/RCP6.csv")
megabiomes <- read.csv("data/cleaned/RCP6_mega.csv")

#List columns
slices <- c("X2000.2019", "X2020.2039", "X2040.2059", "X2060.2079", "X2080.2099",
            "X2100.2119", "X2120.2139", "X2140.2159", "X2160.2179", "X2180.2199",
            "X2200.2219", "X2220.2239", "X2240.2259", "X2260.2279", "X2280.2299",
            "X2300.2319", "X2320.2339", "X2340.2359", "X2360.2379", "X2380.2399",
            "X2400.2419", "X2420.2439", "X2440.2459", "X2460.2479", "X2480.2499")
midpoints <- seq(from = 2010, to = 2490, by = 20)

#Count the biomes in each time slice and convert to a proportion
change_counts <- seq(1, 28, 1)
mega_counts <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")

for (i in 1:length(slices)){
  #Designate slice
  one_bin <- data_sym(slices[i])
  
  #Count number of cells of each biome in that slice
  biome_counts <- count(biomes, !!one_bin)
  megabiome_counts <- count(megabiomes, !!one_bin)
  
  #If biomes are missing, add them as 0s
  colnames(biome_counts) <- c("biomes", "n")
  for(j in 1:28) {
    if ((j %in% biome_counts$biomes) == F)
    {biome_counts <- rbind(biome_counts, c(j, 0))}
  }
  biome_counts <- arrange(biome_counts, biomes)
  
  #Convert counts to proportions
  biome_counts <- mutate(biome_counts, proportion = n/sum(n))
  megabiome_counts <- mutate(megabiome_counts, proportion = n/sum(n))
  
  #Pull values and bind to overall results
  biome_counts <- biome_counts$proportion
  megabiome_counts <- megabiome_counts$proportion
  change_counts <- cbind(change_counts, biome_counts)
  mega_counts <- cbind(mega_counts, megabiome_counts)
}

#Convert results to data frames and clean column names
change_counts <- as.data.frame(change_counts)
colnames(change_counts) <- c("biome", midpoints)

mega_counts <- as.data.frame(mega_counts)
colnames(mega_counts) <- c("megabiome", midpoints)
