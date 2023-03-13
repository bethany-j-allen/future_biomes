#Bethany Allen   7th July 2022
#Code to calculate the proportion of grid cells attributed to each biome over time

library(tidyverse)
library(rlang)

#Read in biome IDs
biomes <- read.csv("data/cleaned/RCP6_all.csv")
megabiomes <- read.csv("data/cleaned/RCP6_mega_all.csv")

#Read in biome conversion
conversion <- read.table("data/biome_conversion.txt", sep = ",")

#List columns
slices <- c("X2000.2019", "X2020.2039", "X2040.2059", "X2060.2079", "X2080.2099",
            "X2100.2119", "X2120.2139", "X2140.2159", "X2160.2179", "X2180.2199",
            "X2200.2219", "X2220.2239", "X2240.2259", "X2260.2279", "X2280.2299",
            "X2300.2319", "X2320.2339", "X2340.2359", "X2360.2379", "X2380.2399",
            "X2400.2419", "X2420.2439", "X2440.2459", "X2460.2479", "X2480.2499")
megabiome_codes <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")
midpoints <- seq(from = 2010, to = 2490, by = 20)

#Add up total terrestrial area
total_area <- sum(biomes$area_km2)

#Count the biomes in each time slice and convert to a proportion
change_area <- seq(1, 28, 1)
mega_area <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")

for (i in 1:length(slices)){
  #Designate slice
  one_bin <- data_sym(slices[i])
  
  #Add up total area covered by each biome
  biome_areas <- c(); megabiome_areas <- c()
  for (k in 1:28) {
    one_biome <- filter(biomes, !!one_bin == k)
    biome_area <- sum(one_biome$area_km2)
    biome_areas[k] <- biome_area
    if (k < (length(megabiome_codes) + 1)) {
      one_megabiome <- filter(megabiomes, !!one_bin == megabiome_codes[k])
      megabiome_area <- sum(one_megabiome$area_km2)
      megabiome_areas[k] <- megabiome_area
    }
  }
  
  #Convert areas to proportions
  biome_areas <- biome_areas/total_area
  megabiome_areas <- megabiome_areas/total_area
  
  #Pull values and bind to overall results
  change_area <- cbind(change_area, biome_areas)
  mega_area <- cbind(mega_area, megabiome_areas)
}

#Convert results to data frames and clean column names
change_area <- as.data.frame(change_area)
colnames(change_area) <- c("biome", midpoints)
change_area <- pivot_longer(change_area, !biome)

mega_area <- as.data.frame(mega_area)
colnames(mega_area) <- c("megabiome", midpoints)
mega_area <- pivot_longer(mega_area, !megabiome)

change_area$value <- as.numeric(change_area$value)
mega_area$value <- as.numeric(mega_area$value)

#Convert biome numbers to labels
change_area <- left_join(change_area, conversion, by = c("biome" = "V1"))
conversion <- distinct(conversion, V3, .keep_all = T)
mega_area <- left_join(mega_area, conversion, by = c("megabiome" = "V3"))

#Plot results
ggplot(data = change_area, aes(x = name, y = value, fill = V2)) +
  geom_bar(stat = "identity", col = "black") +
  xlab("Year") + ylab("Proportion of terrestrial area") +
  theme_classic()

ggplot(data = mega_area, aes(x = name, y = value, fill = V4)) +
  geom_bar(stat = "identity", col = "black") +
  xlab("Year") + ylab("Proportion of terrestrial area") +
  theme_classic()
ggsave("Terrestrial area.pdf", width = 10, height = 6, dpi = 600)
