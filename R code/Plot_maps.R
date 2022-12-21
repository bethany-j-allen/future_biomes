#Bethany Allen   9th Dec 2022
#Code to plot maps of biome distribution over time

library(tidyverse)

#Read in biome IDs
biomes <- read.csv("data/cleaned/RCP6.csv")
megabiomes <- read.csv("data/cleaned/RCP6_mega.csv")

#Read in biome conversion
conversion <- read.table("data/biome_conversion.txt", sep = ",")

#List columns
slices <- c("X2000.2019", "X2020.2039", "X2040.2059", "X2060.2079", "X2080.2099",
            "X2100.2119", "X2120.2139", "X2140.2159", "X2160.2179", "X2180.2199",
            "X2200.2219", "X2220.2239", "X2240.2259", "X2260.2279", "X2280.2299",
            "X2300.2319", "X2320.2339", "X2340.2359", "X2360.2379", "X2380.2399",
            "X2400.2419", "X2420.2439", "X2440.2459", "X2460.2479", "X2480.2499")
midpoints <- seq(from = 2010, to = 2490, by = 20)
megabiome_tags <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")

#Plot one slice
one_slice <- select(biomes, lon, lat, area_km2, slice = X2000.2019)
one_slice <- left_join(one_slice, conversion, by = c("slice" = "V1"))

ggplot(data = one_slice, aes(x = lon, y = lat, group = V2, col = V2)) +
  geom_point() +
  xlab("Longitude") + ylab("Latitude") +
  theme_classic()

one_slice_m <- select(megabiomes, lon, lat, area_km2, slice = X2000.2019)
one_slice_m <- left_join(one_slice_m, conversion, by = c("slice" = "V3"))

ggplot(data = one_slice_m, aes(x = lon, y = lat, group = V4, col = V4)) +
  geom_point() +
  xlab("Longitude") + ylab("Latitude") +
  theme_classic()

#Plot difference between slice and the present

