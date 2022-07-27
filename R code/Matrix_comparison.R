#Bethany Allen   6th June 2022
#Code to calculate the number of grid cells which have changed between slices

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

#Count the biome changes between adjacent time slices
change_counts <- c(); mega_counts <- c()

for (i in 1:(length(slices)-1)){
  change_counts[i] <- length(which(biomes[,i+3] != biomes[,i+4]))
  mega_counts[i] <- length(which(megabiomes[,i+3] != megabiomes[,i+4]))
}

#Add slice midpoints to plot against
midpoints <- seq(from = 2020, to = 2480, by = 20)
to_plot_biomes <- as.data.frame(cbind(midpoints, change_counts))
to_plot_mega <- as.data.frame(cbind(midpoints, mega_counts))

#Plot
ggplot(data = to_plot_biomes, aes(x = midpoints, y = change_counts)) +
  geom_line() +
  xlab("Year") + ylab("Number of grid cell biome transitions") +
  theme_classic()

ggplot(data = to_plot_mega, aes(x = midpoints, y = mega_counts)) +
  geom_line() +
  xlab("Year") + ylab("Number of grid cell megabiome transitions") +
  theme_classic()


#Split results into latitude bands
high_lat <- filter(biomes, between(lat, 60, 90))
high_lat <- rbind(high_lat, filter(biomes, between(lat, -90, -60)))
high_lat_m <- filter(megabiomes, between(lat, 60, 90))
high_lat_m <- rbind(high_lat_m, filter(megabiomes, between(lat, -90, -60)))

mid_lat <- filter(biomes, between(lat, 30, 60))
mid_lat <- rbind(mid_lat, filter(biomes, between(lat, -60, -30)))
mid_lat_m <- filter(megabiomes, between(lat, 30, 60))
mid_lat_m <- rbind(mid_lat_m, filter(megabiomes, between(lat, -60, -30)))

low_lat <- filter(biomes, between(lat, -30, 30))
low_lat_m <- filter(megabiomes, between(lat, -30, 30))

#Count the biome changes between adjacent time slices
changes_high <- c(); changes_mid <- c(); changes_low <- c()
mega_high <- c(); mega_mid <- c(); mega_low <- c()

for (j in 1:(length(slices)-1)){
  changes_high[j] <- length(which(high_lat[,j+3] != high_lat[,j+4]))
  changes_mid[j] <- length(which(mid_lat[,j+3] != mid_lat[,j+4]))
  changes_low[j] <- length(which(low_lat[,j+3] != low_lat[,j+4]))
  mega_high[j] <- length(which(high_lat_m[,j+3] != high_lat_m[,j+4]))
  mega_mid[j] <- length(which(mid_lat_m[,j+3] != mid_lat_m[,j+4]))
  mega_low[j] <- length(which(low_lat_m[,j+3] != low_lat_m[,j+4]))
}

#Add slice midpoints to plot against
lat_plot <- as.data.frame(cbind(midpoints, changes_high, changes_mid, changes_low))
lat_plot <- pivot_longer(lat_plot, !midpoints, names_to = "latitude",
                         names_prefix = "changes_")

lat_plot_m <- as.data.frame(cbind(midpoints, mega_high, mega_mid, mega_low))
lat_plot_m <- pivot_longer(lat_plot_m, !midpoints, names_to = "latitude",
                         names_prefix = "mega_")

#Plot
ggplot(data = lat_plot, aes(x = midpoints, y = value, group = latitude, colour = latitude)) +
  geom_line() +
  xlab("Year") + ylab("Number of grid cell biome transitions") +
  theme_classic()

ggplot(data = lat_plot_m, aes(x = midpoints, y = value, group = latitude, colour = latitude)) +
  geom_line() +
  xlab("Year") + ylab("Number of grid cell megabiome transitions") +
  theme_classic()
