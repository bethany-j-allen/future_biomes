#Bethany Allen   26th July 2022
#Code to calculate the amount of overlap of biome cells between time slices
#  (number of cells which were not occupied by same biome in last slice)

library(tidyverse)

#Read in biome IDs
biomes <- read.csv("data/cleaned/RCP6.csv")
megabiomes <- read.csv("data/cleaned/RCP6_mega.csv")


#List columns
slices <- c("X2000.2019", "X2020.2039", "X2040.2059", "X2060.2079", "X2080.2099",
            "X2100.2119", "X2120.2139", "X2140.2159", "X2160.2179", "X2180.2199",
            "X2200.2219", "X2220.2239", "X2240.2259", "X2260.2279", "X2280.2299",
            "X2300.2319", "X2320.2339", "X2340.2359", "X2360.2379", "X2380.2399",
            "X2400.2419", "X2420.2439", "X2440.2459", "X2460.2479", "X2480.2499")
midpoints <- seq(from = 2020, to = 2480, by = 20)

#Count the biomes in each time slice and convert to a proportion
biome_tags <- seq(1, 28, 1)
megabiome_tags <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")

#Count the biome changes between adjacent time slices
overlap_prop <- data.frame(); overlap_prop_m <- data.frame()

for (j in 1:length(biome_tags)){
  for (i in 1:(length(slices)-1)){
    biome_cells <- biomes[which(biomes[,i+4] == j),]
    overlap_prop[j,i] <- (length(which(biome_cells[,i+3] == j)))/nrow(biome_cells)
    
    if(j < 10){
      megabiome_cells <- megabiomes[which(megabiomes[,i+4] == megabiome_tags[j]),]
      overlap_prop_m[j,i] <- (length(which(megabiome_cells[,i+3] == megabiome_tags[j])))/nrow(megabiome_cells)
      }
    }
}

#Rotate data frames and clean column names
colnames(overlap_prop) <- midpoints
overlap_prop$biome <- biome_tags
overlap_prop <- pivot_longer(overlap_prop, !biome)

colnames(overlap_prop_m) <- midpoints
overlap_prop_m$megabiome <- megabiome_tags
overlap_prop_m <- pivot_longer(overlap_prop_m, !megabiome)

overlap_prop$biome <- as.factor(overlap_prop$biome)

#Plot results
ggplot(data = overlap_prop, aes(x = name, y = value, group = 1)) +
  geom_line() +
  facet_wrap( ~ biome, ncol = 7) +
  xlab("Year") + ylab("Proportion of grid cells") +
  theme_classic()

ggplot(data = overlap_prop_m, aes(x = name, y = value, group = 1)) +
  geom_line() +
  facet_wrap( ~ megabiome, ncol = 3) +
  xlab("Year") + ylab("Proportion of grid cells") +
  theme_classic()
