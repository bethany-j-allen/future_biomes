#Bethany Allen   6th June 2022
#Code to calculate the number of grid cells which have changed between slices

library(tidyverse)

#List columns
slices <- c("X2000.2019", "X2020.2039", "X2040.2059", "X2060.2079", "X2080.2099",
            "X2100.2119", "X2120.2139", "X2140.2159", "X2160.2179", "X2180.2199",
            "X2200.2219", "X2220.2239", "X2240.2259", "X2260.2279", "X2280.2299",
            "X2300.2319", "X2320.2339", "X2340.2359", "X2360.2379", "X2380.2399",
            "X2400.2419", "X2420.2439", "X2440.2459", "X2460.2479", "X2480.2499")

#List RCPs
RCPs <- c("RCP2.6", "RCP4.5", "RCP6")

to_plot_biomes <- c(); to_plot_mega <- c()
to_plot_biomes_a <- c(); to_plot_mega_a <- c()

for (k in 1:length(RCPs)){
  #Read in biome IDs
  biomes <- read.csv(paste0("data/cleaned/", RCPs[k], ".csv"))
  megabiomes <- read.csv(paste0("data/cleaned/", RCPs[k], "_mega.csv"))
  
  #Add up total terrestrial area
  if (k == 1) {total_area <- sum(biomes$area_km2)}

  #Count the biome changes between adjacent time slices, and normalise
  change_counts <- c(); mega_counts <- c()
  change_area <- c(); mega_area <- c()

  for (i in 1:(length(slices)-1)){
    change_counts[i] <- length(which(biomes[,i+3] != biomes[,i+4])) / (nrow(biomes) - 1)
    mega_counts[i] <- length(which(megabiomes[,i+3] != megabiomes[,i+4])) / (nrow(biomes) - 1)
    change_area[i] <- sum(biomes[which(biomes[,i+3] != biomes[,i+4]), 3]) / total_area
    mega_area[i] <- sum(biomes[which(megabiomes[,i+3] != megabiomes[,i+4]), 3]) / total_area
  }

  #Add slice midpoints to plot against
  midpoints <- seq(from = 2020, to = 2480, by = 20)
  to_plot_biomes <- rbind(to_plot_biomes,
                          as.data.frame(cbind(midpoints, change_counts, RCPs[k])))
  to_plot_mega <- rbind(to_plot_mega,
                        as.data.frame(cbind(midpoints, mega_counts, RCPs[k])))
  to_plot_biomes_a <- rbind(to_plot_biomes_a,
                            as.data.frame(cbind(midpoints, change_area, RCPs[k])))
  to_plot_mega_a <- rbind(to_plot_mega_a,
                          as.data.frame(cbind(midpoints, mega_area, RCPs[k])))
}

to_plot_biomes$change_counts <- as.numeric(to_plot_biomes$change_counts)
to_plot_mega$mega_counts <- as.numeric(to_plot_mega$mega_counts)
to_plot_biomes_a$change_area <- as.numeric(to_plot_biomes_a$change_area)
to_plot_mega_a$mega_area <- as.numeric(to_plot_mega_a$mega_area)

#Plot
ggplot(data = to_plot_biomes, aes(x = midpoints, y = change_counts,
                                  group = V3, col = V3)) +
    geom_line() +
    xlab("Year") + ylab("Proportion of grid cells undergoing biome transitions") +
    theme_classic()

ggplot(data = to_plot_mega, aes(x = midpoints, y = mega_counts,
                                group = V3, col = V3)) +
  geom_line() +
  xlab("Year") + ylab("Proportion of grid cells undergoing megabiome transitions") +
  theme_classic()

ggplot(data = to_plot_biomes_a, aes(x = midpoints, y = change_area,
                                    group = V3, col = V3)) +
  geom_line() +
  xlab("Year") + ylab("Proportion of terrestrial area undergoing biome transitions") +
  theme_classic()

ggplot(data = to_plot_mega_a, aes(x = midpoints, y = mega_area,
                                  group = V3, col = V3)) +
  geom_line() +
  xlab("Year") + ylab("Proportion of terrestrial area undergoing megabiome transitions") +
  theme_classic()


#Read in one RCP
biomes <- read.csv(paste0("data/cleaned/RCP6.csv"))
megabiomes <- read.csv(paste0("data/cleaned/RCP6_mega.csv"))

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

#Calculate area for each latitude band
high_lat_area <- sum(high_lat$area_km2)
mid_lat_area <- sum(mid_lat$area_km2)
low_lat_area <- sum(low_lat$area_km2)

#Count the biome changes between adjacent time slices
changes_high <- c(); changes_mid <- c(); changes_low <- c()
mega_high <- c(); mega_mid <- c(); mega_low <- c()
changes_high_a <- c(); changes_mid_a <- c(); changes_low_a <- c()
mega_high_a <- c(); mega_mid_a <- c(); mega_low_a <- c()


for (j in 1:(length(slices)-1)){
  changes_high[j] <- length(which(high_lat[,j+3] != high_lat[,j+4])) / (nrow(high_lat) - 1)
  changes_mid[j] <- length(which(mid_lat[,j+3] != mid_lat[,j+4])) / (nrow(mid_lat) - 1)
  changes_low[j] <- length(which(low_lat[,j+3] != low_lat[,j+4])) / (nrow(low_lat) - 1)
  mega_high[j] <- length(which(high_lat_m[,j+3] != high_lat_m[,j+4])) / (nrow(high_lat) - 1)
  mega_mid[j] <- length(which(mid_lat_m[,j+3] != mid_lat_m[,j+4])) / (nrow(mid_lat) - 1)
  mega_low[j] <- length(which(low_lat_m[,j+3] != low_lat_m[,j+4])) / (nrow(low_lat) - 1)
  changes_high_a[j] <- sum(high_lat[which(high_lat[,j+3] != high_lat[,j+4]), 3]) / high_lat_area
  changes_mid_a[j] <- sum(mid_lat[which(mid_lat[,j+3] != mid_lat[,j+4]), 3]) / mid_lat_area
  changes_low_a[j] <- sum(low_lat[which(low_lat[,j+3] != low_lat[,j+4]), 3]) / low_lat_area
  mega_high_a[j] <- sum(high_lat_m[which(high_lat_m[,j+3] != high_lat_m[,j+4]), 3]) / high_lat_area
  mega_mid_a[j] <- sum(mid_lat_m[which(mid_lat_m[,j+3] != mid_lat_m[,j+4]), 3]) / mid_lat_area
  mega_low_a[j] <- sum(low_lat_m[which(low_lat_m[,j+3] != low_lat_m[,j+4]), 3]) / low_lat_area
}

#Add slice midpoints to plot against
lat_plot <- as.data.frame(cbind(midpoints, changes_high, changes_mid, changes_low))
lat_plot <- pivot_longer(lat_plot, !midpoints, names_to = "latitude",
                         names_prefix = "changes_")

lat_plot_m <- as.data.frame(cbind(midpoints, mega_high, mega_mid, mega_low))
lat_plot_m <- pivot_longer(lat_plot_m, !midpoints, names_to = "latitude",
                         names_prefix = "mega_")

lat_plot_a <- as.data.frame(cbind(midpoints, changes_high_a, changes_mid_a, changes_low_a))
lat_plot_a <- pivot_longer(lat_plot_a, !midpoints, names_to = "latitude",
                         names_prefix = "changes_")

lat_plot_m_a <- as.data.frame(cbind(midpoints, mega_high_a, mega_mid_a, mega_low_a))
lat_plot_m_a <- pivot_longer(lat_plot_m_a, !midpoints, names_to = "latitude",
                           names_prefix = "mega_")

#Plot
ggplot(data = lat_plot, aes(x = midpoints, y = value, group = latitude, colour = latitude)) +
  geom_line() +
  xlab("Year") + ylab("Proportion of grid cells undergoing biome transitions") +
  theme_classic()

ggplot(data = lat_plot_m, aes(x = midpoints, y = value, group = latitude, colour = latitude)) +
  geom_line() +
  xlab("Year") + ylab("Proportion of grid cells undergoing megabiome transitions") +
  theme_classic()

ggplot(data = lat_plot_a, aes(x = midpoints, y = value, group = latitude, colour = latitude)) +
  geom_line() +
  xlab("Year") + ylab("Proportion of terrestrial area undergoing biome transitions") +
  theme_classic()

ggplot(data = lat_plot_m_a, aes(x = midpoints, y = value, group = latitude, colour = latitude)) +
  geom_line() +
  xlab("Year") + ylab("Proportion of terrestrial area undergoing megabiome transitions") +
  theme_classic()
ggsave("Transitions by latitude.pdf", width = 10, height = 6, dpi = 600)
