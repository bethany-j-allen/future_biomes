#Bethany Allen   23rd June 2022
#Code to calculate the movement of biome centroids through time

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
midpoints <- seq(from = 2010, to = 2490, by = 20)
megabiome_tags <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")

#Determine the biome centroids through time
biome_ranges <- data.frame()

for (i in 1:28){
  for (j in 1:(length(slices))){
    #Filter to cells containing i biome in j time slice
    biome_cells <- biomes[(which(biomes[,j+3] == i)),c(1:3, (j+3))]
    
    #Split into hemispheres
    n_hem_cells <- filter(biome_cells, lat > 0)
    s_hem_cells <- filter(biome_cells, lat < 0)
    
    #For each hemisphere, determine max, min and median for long and lat
    if (nrow(n_hem_cells) > 0){
      n_lon_max <- max(n_hem_cells$lon)
      n_lon_min <- min(n_hem_cells$lon)
      n_lon_mid <- (n_lon_max + n_lon_min) / 2
      n_lat_max <- max(n_hem_cells$lat)
      n_lat_min <- min(n_hem_cells$lat)
      n_lat_mid <- (n_lat_max + n_lat_min) / 2
      biome_ranges <- rbind(biome_ranges, c(i, midpoints[j], "N", n_lon_max, n_lon_mid,
                                            n_lon_min, n_lat_max, n_lat_mid, n_lat_min))
    }
    if (nrow(s_hem_cells) > 0){
      s_lon_max <- max(s_hem_cells$lon)
      s_lon_min <- min(s_hem_cells$lon)
      s_lon_mid <- (s_lon_max + s_lon_min) / 2
      s_lat_max <- max(s_hem_cells$lat)
      s_lat_min <- min(s_hem_cells$lat)
      s_lat_mid <- (s_lat_max + s_lat_min) / 2
      biome_ranges <- rbind(biome_ranges, c(i, midpoints[j], "S", s_lon_max, s_lon_mid,
                                            s_lon_min, s_lat_max, s_lat_mid, s_lat_min))
    }
  }
}

#Add column names
colnames(biome_ranges) <- c("biome", "slice", "hemisphere", "lon_max", "lon_mid", "lon_min",
                            "lat_max", "lat_mid", "lat_min")

#Change data type
biome_ranges$biome <- as.numeric(biome_ranges$biome)
biome_ranges$slice <- as.numeric(biome_ranges$slice)
biome_ranges$lon_max <- as.numeric(biome_ranges$lon_max)
biome_ranges$lon_mid <- as.numeric(biome_ranges$lon_mid)
biome_ranges$lon_min <- as.numeric(biome_ranges$lon_min)
biome_ranges$lat_max <- as.numeric(biome_ranges$lat_max)
biome_ranges$lat_mid <- as.numeric(biome_ranges$lat_mid)
biome_ranges$lat_min <- as.numeric(biome_ranges$lat_min)

#Plot latitudes
ggplot(data = biome_ranges) +
  geom_ribbon(aes(x = slice, ymin = lat_min, ymax = lat_max, 
                  group = hemisphere), alpha = 0.5) +
  geom_line(aes(x = slice, y = lat_mid, group = hemisphere)) +
  facet_wrap( ~ biome, ncol = 7) +
  xlab("Year") + ylab("Latitudinal range of biome") +
  theme_classic()

#Plot centroid movement
ggplot(data = biome_ranges) +
  geom_point(aes(x = lon_mid, y = lat_mid, group = hemisphere, col = slice,
                 size = 2, alpha = 0.5)) +
  geom_hline(aes(yintercept = 0), colour = "black") +
  facet_wrap( ~ biome, ncol = 7) +
  xlab("Longitudinal midpoint of biome") + ylab("Latitudinal midpoint of biome") +
  theme_classic()


#Determine the megabiome centroids through time
megabiome_ranges <- data.frame()

for (k in 1:9){
  for (l in 1:(length(slices))){
    #Filter to cells containing i biome in j time slice
    megabiome_cells <- megabiomes[(which(megabiomes[,l+3] == megabiome_tags[k])),c(1:3, (l+3))]
    
    #Split into hemispheres
    n_hem_m_cells <- filter(megabiome_cells, lat > 0)
    s_hem_m_cells <- filter(megabiome_cells, lat < 0)
    
    #For each hemisphere, determine max, min and median for long and lat
    if (nrow(n_hem_m_cells) > 0){
      n_lon_m_max <- max(n_hem_m_cells$lon)
      n_lon_m_min <- min(n_hem_m_cells$lon)
      n_lon_m_mid <- (n_lon_m_max + n_lon_m_min) / 2
      n_lat_m_max <- max(n_hem_m_cells$lat)
      n_lat_m_min <- min(n_hem_m_cells$lat)
      n_lat_m_mid <- (n_lat_m_max + n_lat_m_min) / 2
      megabiome_ranges <- rbind(megabiome_ranges, c(megabiome_tags[k], midpoints[l], "N", n_lon_m_max, n_lon_m_mid,
                                            n_lon_m_min, n_lat_m_max, n_lat_m_mid, n_lat_m_min))
    }
    if (nrow(s_hem_m_cells) > 0){
      s_lon_m_max <- max(s_hem_m_cells$lon)
      s_lon_m_min <- min(s_hem_m_cells$lon)
      s_lon_m_mid <- (s_lon_m_max + s_lon_m_min) / 2
      s_lat_m_max <- max(s_hem_m_cells$lat)
      s_lat_m_min <- min(s_hem_m_cells$lat)
      s_lat_m_mid <- (s_lat_m_max + s_lat_m_min) / 2
      megabiome_ranges <- rbind(megabiome_ranges, c(megabiome_tags[k], midpoints[l], "S", s_lon_m_max, s_lon_m_mid,
                                            s_lon_m_min, s_lat_m_max, s_lat_m_mid, s_lat_m_min))
    }
  }
}

#Add column names
colnames(megabiome_ranges) <- c("megabiome", "slice", "hemisphere", "lon_max", "lon_mid", "lon_min",
                            "lat_max", "lat_mid", "lat_min")

#Change data type
megabiome_ranges$slice <- as.numeric(megabiome_ranges$slice)
megabiome_ranges$lon_max <- as.numeric(megabiome_ranges$lon_max)
megabiome_ranges$lon_mid <- as.numeric(megabiome_ranges$lon_mid)
megabiome_ranges$lon_min <- as.numeric(megabiome_ranges$lon_min)
megabiome_ranges$lat_max <- as.numeric(megabiome_ranges$lat_max)
megabiome_ranges$lat_mid <- as.numeric(megabiome_ranges$lat_mid)
megabiome_ranges$lat_min <- as.numeric(megabiome_ranges$lat_min)

#Plot latitudes
ggplot(data = megabiome_ranges) +
  geom_ribbon(aes(x = slice, ymin = lat_min, ymax = lat_max, 
                  group = hemisphere), alpha = 0.5) +
  geom_line(aes(x = slice, y = lat_mid, group = hemisphere)) +
  facet_wrap( ~ megabiome, ncol = 3) +
  xlab("Year") + ylab("Latitudinal range of biome") +
  theme_classic()

#Plot centroid movement
ggplot(data = megabiome_ranges) +
  geom_point(aes(x = lon_mid, y = lat_mid, group = hemisphere, col = slice,
                 size = 2, alpha = 0.5)) +
  geom_hline(aes(yintercept = 0), colour = "black") +
  facet_wrap( ~ megabiome, ncol = 3) +
  xlab("Longitudinal midpoint of biome") + ylab("Latitudinal range of biome") +
  theme_classic()
