#Bethany Allen   7th July 2022
#Code to calculate the proportion of grid cells attributed to each biome over time

library(tidyverse)
library(rlang)

#List filenames
filenames <- c("RCP2.6_all", "RCP2.6_no_urban", "RCP2.6_no_human",
               "RCP4.5_all", "RCP4.5_no_urban", "RCP4.5_no_human",
               "RCP6_all", "RCP6_no_urban", "RCP6_no_human")

mega_filenames <- c("RCP2.6_mega_all", "RCP2.6_mega_no_urban",
                    "RCP2.6_mega_no_human", "RCP4.5_mega_all",
                    "RCP4.5_mega_no_urban", "RCP4.5_mega_no_human",
                    "RCP6_mega_all", "RCP6_mega_no_urban", "RCP6_mega_no_human")

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

biome_shares <- c(); megabiome_shares <- c()

for (j in 1:length(filenames)){

  #Read in biome IDs
  biomes <- read.csv(paste0("data/cleaned/", filenames[j], ".csv"))
  megabiomes <- read.csv(paste0("data/cleaned/", mega_filenames[j], ".csv"))

  #Add up total terrestrial area
  total_area <- sum(biomes$area_km2)

  #Remove NAs (needed for no_urban and no_human)
  biomes <- na.omit(biomes)
  megabiomes <- na.omit(megabiomes)

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
  change_area$RCP <- sub("_.*", "", filenames[j])
  change_area$footprint <- sub(".*_", "", filenames[j])
  biome_shares <- rbind(biome_shares, change_area)

  mega_area <- as.data.frame(mega_area)
  colnames(mega_area) <- c("megabiome", midpoints)
  mega_area <- pivot_longer(mega_area, !megabiome)
  mega_area$RCP <- sub("_.*", "", mega_filenames[j])
  mega_area$footprint <- sub(".*_", "", mega_filenames[j])
  megabiome_shares <- rbind(megabiome_shares, mega_area)
  
  #Track progress
  print(j)
}

biome_shares$name <- as.numeric(biome_shares$name)
megabiome_shares$name <- as.numeric(megabiome_shares$name)
biome_shares$value <- as.numeric(biome_shares$value)
megabiome_shares$value <- as.numeric(megabiome_shares$value)

biome_shares$footprint <- factor(biome_shares$footprint,
                                   levels = c("all", "urban", "human"))
megabiome_shares$footprint <- factor(megabiome_shares$footprint,
                                 levels = c("all", "urban", "human"))

#Convert biome numbers to labels
#biome_shares <- left_join(biome_shares, conversion, by = c("biome" = "V1"))
#conversion <- distinct(conversion, V3, .keep_all = T)
#megabiome_shares <- left_join(megabiome_shares, conversion, by = c("megabiome" = "V3"))
biome_shares$biome <- as.factor(biome_shares$biome)

#Plot results
ggplot(data = biome_shares, aes(x = name, y = value, fill = biome)) +
  geom_bar(stat = "identity", col = "black") +
  facet_grid(RCP ~ footprint) +
  xlab("Year") + ylab("Proportion of terrestrial area") +
  scale_x_continuous(guide = guide_axis(angle = 90)) +
  theme_classic() 
ggsave(paste0("figures/Biome area.pdf"), width = 10, height = 6, dpi = 600)

ggplot(data = megabiome_shares, aes(x = name, y = value, fill = megabiome)) +
  geom_bar(stat = "identity", col = "black") +
  facet_grid(RCP ~ footprint) +
  xlab("Year") + ylab("Proportion of terrestrial area") +
  scale_x_continuous(guide = guide_axis(angle = 90)) +
  theme_classic()
ggsave(paste0("figures/Megabiome area.pdf"), width = 10, height = 6, dpi = 600)
