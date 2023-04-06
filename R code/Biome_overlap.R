#Bethany Allen   26th July 2022
#Code to calculate the amount of overlap of biome cells between time slices
#  (area which was occupied by same biome in last slice)

library(tidyverse)

#List filenames
filenames <- c("RCP2.6_all", "RCP2.6_no_urban", "RCP2.6_no_human",
               "RCP4.5_all", "RCP4.5_no_urban", "RCP4.5_no_human",
               "RCP6_all", "RCP6_no_urban", "RCP6_no_human")

mega_filenames <- c("RCP2.6_mega_all", "RCP2.6_mega_no_urban",
                    "RCP2.6_mega_no_human", "RCP4.5_mega_all",
                    "RCP4.5_mega_no_urban", "RCP4.5_mega_no_human",
                    "RCP6_mega_all", "RCP6_mega_no_urban", "RCP6_mega_no_human")

overlap_area_all <- data.frame(); overlap_area_m_all <- data.frame()

for (k in 1:length(filenames)){
  #Read in biome IDs
  biomes <- read.csv(paste0("data/cleaned/", filenames[k], ".csv"))
  megabiomes <- read.csv(paste0("data/cleaned/", mega_filenames[k], ".csv"))

  #Read in biome conversion
  conversion <- read.table("data/biome_conversion.txt", sep = ",")

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
  
  #Remove NAs (needed for no_urban and no_human)
  biomes <- na.omit(biomes)
  megabiomes <- na.omit(megabiomes)

  #Count the biome changes between adjacent time slices
  overlap_area <- data.frame(); overlap_area_m <- data.frame()
  
  for (j in 1:length(biome_tags)) {
    for (i in 1:(length(slices) - 1)) {
      biome_cells <- biomes[which(biomes[,i+4] == j),]
      biome_area <- sum(biome_cells$area_km2)
      biome_overlap <- biome_cells[which(biome_cells[,i+3] == j),]
      overlap_area[j,i] <- (sum(biome_overlap$area_km2))/biome_area
    
      if(j < 10){
        megabiome_cells <- megabiomes[which(megabiomes[,i+4] == megabiome_tags[j]),]
        megabiome_area <- sum(megabiome_cells$area_km2)
        megabiome_overlap <- megabiome_cells[which(megabiome_cells[,i+3] == megabiome_tags[j]),]
        overlap_area_m[j,i] <- (sum(megabiome_overlap$area_km2))/megabiome_area
      }
    }
  }
  #Add labels
  colnames(overlap_area) <- midpoints
  colnames(overlap_area_m) <- midpoints
  
  overlap_area$biome <- biome_tags
  overlap_area$file <- filenames[k]

  overlap_area_m$megabiome <- megabiome_tags
  overlap_area_m$file <- mega_filenames[k]
  
  overlap_area_all <- rbind(overlap_area_all, overlap_area)
  overlap_area_m_all <- rbind(overlap_area_m_all, overlap_area_m)
}

#Rotate data frames
overlap_area_all <- pivot_longer(overlap_area_all, !c(biome, file))
overlap_area_m_all <- pivot_longer(overlap_area_m_all, !c(megabiome, file))

#Convert biome numbers to labels
overlap_area_all <- left_join(overlap_area_all, conversion, by = c("biome" = "V1"))
conversion <- distinct(conversion, V3, .keep_all = T)
overlap_area_m_all <- left_join(overlap_area_m_all, conversion, by = c("megabiome" = "V3"))

overlap_area_all$name <- as.numeric(overlap_area_all$name)
overlap_area_m_all$name <- as.numeric(overlap_area_m_all$name)

#Plot results
ggplot(data = overlap_area_all, aes(x = name, y = value, group = file, colour = file)) +
  geom_line() +
  facet_wrap( ~ V2, ncol = 7, labeller = label_wrap_gen(multi_line = T)) +
  scale_colour_manual(values = c("firebrick1", "firebrick4", "firebrick3",
                                 "cadetblue1", "cadetblue4", "cadetblue3", 
                                 "green2", "green4", "green3")) +
  xlab("Year") + ylab("Proportion of biome area overlapping with previous slice") +
  scale_x_continuous(guide = guide_axis(angle = 90)) +
  theme_classic()
ggsave("figures/Biome overlap.pdf", width = 10, height = 6, dpi = 600)

ggplot(data = overlap_area_m_all, aes(x = name, y = value, group = file, colour = file)) +
  geom_line() +
  facet_wrap( ~ V4, ncol = 3) +
  scale_colour_manual(values = c("firebrick1", "firebrick4", "firebrick3",
                                 "cadetblue1", "cadetblue4", "cadetblue3", 
                                 "green2", "green4", "green3")) +
  xlab("Year") + ylab("Proportion of megabiome area overlapping with previous slice") +
  scale_x_continuous(guide = guide_axis(angle = 90)) +
  theme_classic()
ggsave("figures/Megabiome overlap.pdf", width = 10, height = 6, dpi = 600)
