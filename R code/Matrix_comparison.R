#Bethany Allen   6th June 2022
#Code to calculate the number of grid cells which have changed between slices

library(tidyverse)

#Read in biome IDs
biomes <- read.csv("data/cleaned/RCP6.csv")

#List columns
slices <- c("X2000.2019", "X2020.2039", "X2040.2059", "X2060.2079", "X2080.2099",
            "X2100.2119", "X2120.2139", "X2140.2159", "X2160.2179", "X2180.2199",
            "X2200.2219", "X2220.2239", "X2240.2259", "X2260.2279", "X2280.2299",
            "X2300.2319", "X2320.2339", "X2340.2359", "X2360.2379", "X2380.2399",
            "X2400.2419", "X2420.2439", "X2440.2459", "X2460.2479", "X2480.2499")

#Count the biome changes between adjacent time slices
change_counts <- c()

for (i in 1:(length(slices)-1)){
  change_counts[i] <- length(which(biomes[,i] != biomes[,i+1]))
}

#Add slice midpoints to plot against
midpoints <- seq(from = 2020, to = 2480, by = 20)
to_plot <- as.data.frame(cbind(midpoints, change_counts))

#Plot
ggplot(data = to_plot, aes(x = midpoints, y = change_counts)) +
  geom_line() +
  xlab("Year") + ylab("Number of grid cell biome transitions") +
  theme_classic()


