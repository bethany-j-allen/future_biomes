#Bethany Allen   6th June 2022
#Code to calculate the terrestrial area which has changed biome between time
# slices, and compared to the present day

library(tidyverse)

#List columns
slices <- c("X2000.2019", "X2020.2039", "X2040.2059", "X2060.2079", "X2080.2099",
            "X2100.2119", "X2120.2139", "X2140.2159", "X2160.2179", "X2180.2199",
            "X2200.2219", "X2220.2239", "X2240.2259", "X2260.2279", "X2280.2299",
            "X2300.2319", "X2320.2339", "X2340.2359", "X2360.2379", "X2380.2399",
            "X2400.2419", "X2420.2439", "X2440.2459", "X2460.2479", "X2480.2499")
midpoints <- seq(from = 2020, to = 2480, by = 20)

#List filenames
filenames <- c("RCP2.6_all", "RCP2.6_no_urban", "RCP2.6_no_human",
               "RCP4.5_all", "RCP4.5_no_urban", "RCP4.5_no_human",
               "RCP6_all", "RCP6_no_urban", "RCP6_no_human")

mega_filenames <- c("RCP2.6_mega_all", "RCP2.6_mega_no_urban",
                    "RCP2.6_mega_no_human", "RCP4.5_mega_all",
                    "RCP4.5_mega_no_urban", "RCP4.5_mega_no_human",
                    "RCP6_mega_all", "RCP6_mega_no_urban", "RCP6_mega_no_human")

#How much change compared to the adjacent time slice?

to_plot_biomes <- c(); to_plot_mega <- c()

for (k in 1:length(filenames)){
  #Read in biome IDs
  biomes <- read.csv(paste0("data/cleaned/", filenames[k], ".csv"))
  megabiomes <- read.csv(paste0("data/cleaned/", mega_filenames[k], ".csv"))
  
  #Add up total terrestrial area included
  total_area <- sum(biomes$area_km2)
  
  #Remove NAs (needed for no_urban and no_human)
  biomes <- na.omit(biomes)
  megabiomes <- na.omit(megabiomes)

  #Count the biome changes between adjacent time slices, and normalise
  change_area <- c(); mega_area <- c()

  for (i in 1:(length(slices)-1)){
    change_area[i] <- sum(biomes[which(biomes[,i+3] != biomes[,i+4]), 3]) /total_area
    mega_area[i] <- sum(biomes[which(megabiomes[,i+3] != megabiomes[,i+4]), 3]) / total_area
  }

  #Add slice midpoints to plot against
  to_plot_biomes <- rbind(to_plot_biomes,
                          as.data.frame(cbind(midpoints, change_area,
                                              filenames[k])))
  to_plot_mega <- rbind(to_plot_mega,
                        as.data.frame(cbind(midpoints, mega_area,
                                            mega_filenames[k])))
  
  #Track progress
  print(k)
}

to_plot_biomes$change_area <- as.numeric(to_plot_biomes$change_area)
to_plot_mega$mega_area <- as.numeric(to_plot_mega$mega_area)

#Plot
ggplot(data = to_plot_biomes, aes(x = midpoints, y = change_area,
                                    group = V3, col = V3)) +
  geom_line() +
  scale_colour_manual(values = c("firebrick1", "firebrick4", "firebrick3",
                                      "cadetblue1", "cadetblue4", "cadetblue3", 
                                      "green2", "green4", "green3")) +
  xlab("Year") + ylab("Proportion of terrestrial area undergoing biome transitions between slices") +
  theme_classic()
ggsave("figures/Biome transitions between slices.pdf", width = 10, height = 6, dpi = 600)

ggplot(data = to_plot_mega, aes(x = midpoints, y = mega_area,
                                  group = V3, col = V3)) +
  geom_line() +
  scale_colour_manual(values = c("firebrick1", "firebrick4", "firebrick3",
                                      "cadetblue1", "cadetblue4", "cadetblue3", 
                                      "green2", "green4", "green3")) +
  xlab("Year") + ylab("Proportion of terrestrial area undergoing megabiome transitions between slices") +
  theme_classic()
ggsave("figures/Megabiome transitions between slices.pdf", width = 10, height = 6, dpi = 600)

#Plot change compared to adjacent time slice by latitude

RCPs <- c(2.6, 4.5, 6)
lat_plot_all <- c(); mega_lat_plot_all <- c()

#Read in one RCP
for (m in 1:3) {
  biomes_all <- read.csv(paste0("data/cleaned/RCP", RCPs[m], "_all.csv"))
  biomes_no_urban <- read.csv(paste0("data/cleaned/RCP", RCPs[m], "_no_urban.csv"))
  biomes_no_human <- read.csv(paste0("data/cleaned/RCP", RCPs[m], "_no_human.csv"))
  megabiomes_all <- read.csv(paste0("data/cleaned/RCP", RCPs[m], "_mega_all.csv"))
  megabiomes_no_urban <- read.csv(paste0("data/cleaned/RCP", RCPs[m], "_mega_no_urban.csv"))
  megabiomes_no_human <- read.csv(paste0("data/cleaned/RCP", RCPs[m], "_mega_no_human.csv"))

  #Split results into latitude bands
  high_lat_all <- filter(biomes_all, between(lat, 60, 90))
  high_lat_all <- rbind(high_lat_all, filter(biomes_all, between(lat, -90, -60)))
  high_lat_no_urban <- filter(biomes_no_urban, between(lat, 60, 90))
  high_lat_no_urban <- rbind(high_lat_no_urban, filter(biomes_no_urban, between(lat, -90, -60)))
  high_lat_no_human <- filter(biomes_no_human, between(lat, 60, 90))
  high_lat_no_human <- rbind(high_lat_no_human, filter(biomes_no_human, between(lat, -90, -60)))

  high_lat_m_all <- filter(megabiomes_all, between(lat, 60, 90))
  high_lat_m_all <- rbind(high_lat_m_all, filter(megabiomes_all, between(lat, -90, -60)))
  high_lat_m_no_urban <- filter(megabiomes_no_urban, between(lat, 60, 90))
  high_lat_m_no_urban <- rbind(high_lat_m_no_urban, filter(megabiomes_no_urban, between(lat, -90, -60)))
  high_lat_m_no_human <- filter(megabiomes_no_human, between(lat, 60, 90))
  high_lat_m_no_human <- rbind(high_lat_m_no_human, filter(megabiomes_no_human, between(lat, -90, -60)))

  mid_lat_all <- filter(biomes_all, between(lat, 30, 60))
  mid_lat_all <- rbind(mid_lat_all, filter(biomes_all, between(lat, -60, -30)))
  mid_lat_no_urban <- filter(biomes_no_urban, between(lat, 30, 60))
  mid_lat_no_urban <- rbind(mid_lat_no_urban, filter(biomes_no_urban, between(lat, -60, -30)))
  mid_lat_no_human <- filter(biomes_no_human, between(lat, 30, 60))
  mid_lat_no_human <- rbind(mid_lat_no_human, filter(biomes_no_human, between(lat, -60, -30)))

  mid_lat_m_all <- filter(megabiomes_all, between(lat, 30, 60))
  mid_lat_m_all <- rbind(mid_lat_m_all, filter(megabiomes_all, between(lat, -60, -30)))
  mid_lat_m_no_urban <- filter(megabiomes_no_urban, between(lat, 30, 60))
  mid_lat_m_no_urban <- rbind(mid_lat_m_no_urban, filter(megabiomes_no_urban, between(lat, -60, -30)))
  mid_lat_m_no_human <- filter(megabiomes_no_human, between(lat, 30, 60))
  mid_lat_m_no_human <- rbind(mid_lat_m_no_human, filter(megabiomes_no_human, between(lat, -60, -30)))

  low_lat_all <- filter(biomes_all, between(lat, -30, 30))
  low_lat_no_urban <- filter(biomes_no_urban, between(lat, -30, 30))
  low_lat_no_human <- filter(biomes_no_human, between(lat, -30, 30))

  low_lat_m_all <- filter(megabiomes_all, between(lat, -30, 30))
  low_lat_m_no_urban <- filter(megabiomes_no_urban, between(lat, -30, 30))
  low_lat_m_no_human <- filter(megabiomes_no_human, between(lat, -30, 30))

  #Calculate area for each latitude band (low versus high res)
  high_lat_area_lr <- sum(high_lat_all$area_km2)
  mid_lat_area_lr <- sum(mid_lat_all$area_km2)
  low_lat_area_lr <- sum(low_lat_all$area_km2)

  high_lat_area_hr <- sum(high_lat_no_urban$area_km2)
  mid_lat_area_hr <- sum(mid_lat_no_urban$area_km2)
  low_lat_area_hr <- sum(low_lat_no_urban$area_km2)

  #Remove NAs from no_human and no_urban
  high_lat_no_urban <- na.omit(high_lat_no_urban)
  high_lat_no_human <- na.omit(high_lat_no_human)
  high_lat_m_no_urban <- na.omit(high_lat_m_no_urban)
  high_lat_m_no_human <- na.omit(high_lat_m_no_human)

  mid_lat_no_urban <- na.omit(mid_lat_no_urban)
  mid_lat_no_human <- na.omit(mid_lat_no_human)
  mid_lat_m_no_urban <- na.omit(mid_lat_m_no_urban)
  mid_lat_m_no_human <- na.omit(mid_lat_m_no_human)

  low_lat_no_urban <- na.omit(low_lat_no_urban)
  low_lat_no_human <- na.omit(low_lat_no_human)
  low_lat_m_no_urban <- na.omit(low_lat_m_no_urban)
  low_lat_m_no_human <- na.omit(low_lat_m_no_human)

  #Count the biome changes between adjacent time slices
  all_high <- c(); all_mid <- c(); all_low <- c()
  no_urban_high <- c(); no_urban_mid <- c(); no_urban_low <- c()
  no_human_high <- c(); no_human_mid <- c(); no_human_low <- c()
  mega_all_high <- c(); mega_all_mid <- c(); mega_all_low <- c()
  mega_no_urban_high <- c(); mega_no_urban_mid <- c(); mega_no_urban_low <- c()
  mega_no_human_high <- c(); mega_no_human_mid <- c(); mega_no_human_low <- c()

  for (j in 1:(length(slices) - 1)) {
    all_high[j] <- sum(high_lat_all[which(high_lat_all[,j+3] != high_lat_all[,j+4]), 3]) / high_lat_area_lr
    no_urban_high[j] <- sum(high_lat_no_urban[which(high_lat_no_urban[,j+3] != high_lat_no_urban[,j+4]), 3]) / high_lat_area_hr
    no_human_high[j] <- sum(high_lat_no_human[which(high_lat_no_human[,j+3] != high_lat_no_human[,j+4]), 3]) / high_lat_area_hr
  
    all_mid[j] <- sum(mid_lat_all[which(mid_lat_all[,j+3] != mid_lat_all[,j+4]), 3]) / mid_lat_area_lr
    no_urban_mid[j] <- sum(mid_lat_no_urban[which(mid_lat_no_urban[,j+3] != mid_lat_no_urban[,j+4]), 3]) / mid_lat_area_hr
    no_human_mid[j] <- sum(mid_lat_no_human[which(mid_lat_no_human[,j+3] != mid_lat_no_human[,j+4]), 3]) / mid_lat_area_hr
  
    all_low[j] <- sum(low_lat_all[which(low_lat_all[,j+3] != low_lat_all[,j+4]), 3]) / low_lat_area_lr
    no_urban_low[j] <- sum(low_lat_no_urban[which(low_lat_no_urban[,j+3] != low_lat_no_urban[,j+4]), 3]) / low_lat_area_hr
    no_human_low[j] <- sum(low_lat_no_human[which(low_lat_no_human[,j+3] != low_lat_no_human[,j+4]), 3]) / low_lat_area_hr
  
    mega_all_high[j] <- sum(high_lat_m_all[which(high_lat_m_all[,j+3] != high_lat_m_all[,j+4]), 3]) / high_lat_area_lr
    mega_no_urban_high[j] <- sum(high_lat_m_no_urban[which(high_lat_m_no_urban[,j+3] != high_lat_m_no_urban[,j+4]), 3]) / high_lat_area_hr
    mega_no_human_high[j] <- sum(high_lat_m_no_human[which(high_lat_m_no_human[,j+3] != high_lat_m_no_human[,j+4]), 3]) / high_lat_area_hr
  
    mega_all_mid[j] <- sum(mid_lat_m_all[which(mid_lat_m_all[,j+3] != mid_lat_m_all[,j+4]), 3]) / mid_lat_area_lr
    mega_no_urban_mid[j] <- sum(mid_lat_m_no_urban[which(mid_lat_m_no_urban[,j+3] != mid_lat_m_no_urban[,j+4]), 3]) / mid_lat_area_hr
    mega_no_human_mid[j] <- sum(mid_lat_m_no_human[which(mid_lat_m_no_human[,j+3] != mid_lat_m_no_human[,j+4]), 3]) / mid_lat_area_hr
  
    mega_all_low[j] <- sum(low_lat_m_all[which(low_lat_m_all[,j+3] != low_lat_m_all[,j+4]), 3]) / low_lat_area_lr
    mega_no_urban_low[j] <- sum(low_lat_m_no_urban[which(low_lat_m_no_urban[,j+3] != low_lat_m_no_urban[,j+4]), 3]) / low_lat_area_hr
    mega_no_human_low[j] <- sum(low_lat_m_no_human[which(low_lat_m_no_human[,j+3] != low_lat_m_no_human[,j+4]), 3]) / low_lat_area_hr
  }

  #Add slice midpoints to plot against
  lat_plot <- as.data.frame(cbind(midpoints, all_high, all_mid, all_low,
                                no_urban_high, no_urban_mid, no_urban_low,
                                no_human_high, no_human_mid, no_human_low))
  lat_plot <- pivot_longer(lat_plot, !midpoints, names_to = "latitude")
  lat_plot$RCP <- RCPs[m]
  lat_plot$footprint <- sub('_[^_]*$', '', lat_plot$latitude)
  lat_plot$latitude <- sub(".*_", "", lat_plot$latitude)
  lat_plot_all <- rbind (lat_plot_all, lat_plot)

  lat_plot_m <- as.data.frame(cbind(midpoints, mega_all_high, mega_all_mid, mega_all_low,
                                  mega_no_urban_high, mega_no_urban_mid, mega_no_urban_low,
                                  mega_no_human_high, mega_no_human_mid, mega_no_human_low))
  lat_plot_m <- pivot_longer(lat_plot_m, !midpoints, names_to = "latitude",
                         names_prefix = "mega_")
  lat_plot_m$RCP <- RCPs[m]
  lat_plot_m$footprint <- sub('_[^_]*$', '', lat_plot_m$latitude)
  lat_plot_m$latitude <- sub(".*_", "", lat_plot_m$latitude)
  mega_lat_plot_all <- rbind(mega_lat_plot_all, lat_plot_m)
  
  #Track progress
  print(m)
}

#Plot
ggplot(data = lat_plot_all, aes(x = midpoints, y = value,
                                group = footprint, colour = footprint)) +
  geom_line() +
  facet_grid(RCP ~ latitude) +
  scale_colour_manual(values = c("firebrick1", "cadetblue1", "green2")) +
  xlab("Year") + ylab("Proportion of terrestrial area undergoing biome transitions between slices") +
  scale_x_continuous(guide = guide_axis(angle = 90)) +
  theme_classic()
ggsave(paste0("figures/Biome transitions between slices by latitude.pdf"), width = 10, height = 6, dpi = 600)

ggplot(data = mega_lat_plot_all, aes(x = midpoints, y = value,
                              group = footprint, colour = footprint)) +
  geom_line() +
  facet_grid(RCP ~ latitude) +
  scale_colour_manual(values = c("firebrick1", "cadetblue1", "green2")) +
  xlab("Year") + ylab("Proportion of terrestrial area undergoing megabiome transitions between slices") +
  scale_x_continuous(guide = guide_axis(angle = 90)) +
  theme_classic()
ggsave(paste0("figures/Megabiome transitions between slices by latitude.pdf"), width = 10, height = 6, dpi = 600)


#How much change compared to the present day?

to_plot_biomes <- c(); to_plot_mega <- c()

for (k in 1:length(filenames)){
  #Read in biome IDs
  biomes <- read.csv(paste0("data/cleaned/", filenames[k], ".csv"))
  megabiomes <- read.csv(paste0("data/cleaned/", mega_filenames[k], ".csv"))
  
  #Remove NAs (needed for no_urban and no_human)
  biomes <- na.omit(biomes)
  megabiomes <- na.omit(megabiomes)
  
  #Add up total terrestrial area
  total_area <- sum(biomes$area_km2)
  
  #Count the biome changes between adjacent time slices, and normalise
  change_area <- c(); mega_area <- c()
  
  for (i in 1:(length(slices)-1)){
    change_area[i] <- sum(biomes[which(biomes[,4] != biomes[,i+4]), 3]) / total_area
    mega_area[i] <- sum(biomes[which(megabiomes[,4] != megabiomes[,i+4]), 3]) / total_area
  }
  
  #Add slice midpoints to plot against
  midpoints <- seq(from = 2030, to = 2490, by = 20)
  to_plot_biomes <- rbind(to_plot_biomes,
                          as.data.frame(cbind(midpoints, change_area, filenames[k])))
  to_plot_mega <- rbind(to_plot_mega,
                        as.data.frame(cbind(midpoints, mega_area, mega_filenames[k])))

  #Track progress
  print(k)
}

to_plot_biomes$change_area <- as.numeric(to_plot_biomes$change_area)
to_plot_mega$mega_area <- as.numeric(to_plot_mega$mega_area)

#Plot
ggplot(data = to_plot_biomes, aes(x = midpoints, y = change_area,
                                    group = V3, col = V3)) +
  geom_line() +
  scale_colour_manual(values = c("firebrick1", "firebrick4", "firebrick3",
                                      "cadetblue1", "cadetblue4", "cadetblue3", 
                                      "green2", "green4", "green3")) +
  xlab("Year") + ylab("Proportion of terrestrial area undergoing biome transitions compared to 2000-2020") +
  theme_classic()
ggsave("figures/Biome transitions from present.pdf", width = 10, height = 6, dpi = 600)

ggplot(data = to_plot_mega, aes(x = midpoints, y = mega_area,
                                  group = V3, col = V3)) +
  geom_line() +
  scale_colour_manual(values = c("firebrick1", "firebrick4", "firebrick3",
                                      "cadetblue1", "cadetblue4", "cadetblue3", 
                                      "green2", "green4", "green3")) +
  xlab("Year") + ylab("Proportion of terrestrial area undergoing megabiome transitions compared to 2000-2020") +
  theme_classic()
ggsave("figures/Megabiome transitions between slices.pdf", width = 10, height = 6, dpi = 600)

#Plot change compared to present day by latitude

RCPs <- c(2.6, 4.5, 6)
lat_plot_all <- c(); mega_lat_plot_all <- c()

#Read in one RCP
for (m in 1:3) {
  biomes_all <- read.csv(paste0("data/cleaned/RCP", RCPs[m], "_all.csv"))
  biomes_no_urban <- read.csv(paste0("data/cleaned/RCP", RCPs[m], "_no_urban.csv"))
  biomes_no_human <- read.csv(paste0("data/cleaned/RCP", RCPs[m], "_no_human.csv"))
  megabiomes_all <- read.csv(paste0("data/cleaned/RCP", RCPs[m], "_mega_all.csv"))
  megabiomes_no_urban <- read.csv(paste0("data/cleaned/RCP", RCPs[m], "_mega_no_urban.csv"))
  megabiomes_no_human <- read.csv(paste0("data/cleaned/RCP", RCPs[m], "_mega_no_human.csv"))
  
  #Split results into latitude bands
  high_lat_all <- filter(biomes_all, between(lat, 60, 90))
  high_lat_all <- rbind(high_lat_all, filter(biomes_all, between(lat, -90, -60)))
  high_lat_no_urban <- filter(biomes_no_urban, between(lat, 60, 90))
  high_lat_no_urban <- rbind(high_lat_no_urban, filter(biomes_no_urban, between(lat, -90, -60)))
  high_lat_no_human <- filter(biomes_no_human, between(lat, 60, 90))
  high_lat_no_human <- rbind(high_lat_no_human, filter(biomes_no_human, between(lat, -90, -60)))

  high_lat_m_all <- filter(megabiomes_all, between(lat, 60, 90))
  high_lat_m_all <- rbind(high_lat_m_all, filter(megabiomes_all, between(lat, -90, -60)))
  high_lat_m_no_urban <- filter(megabiomes_no_urban, between(lat, 60, 90))
  high_lat_m_no_urban <- rbind(high_lat_m_no_urban, filter(megabiomes_no_urban, between(lat, -90, -60)))
  high_lat_m_no_human <- filter(megabiomes_no_human, between(lat, 60, 90))
  high_lat_m_no_human <- rbind(high_lat_m_no_human, filter(megabiomes_no_human, between(lat, -90, -60)))

  mid_lat_all <- filter(biomes_all, between(lat, 30, 60))
  mid_lat_all <- rbind(mid_lat_all, filter(biomes_all, between(lat, -60, -30)))
  mid_lat_no_urban <- filter(biomes_no_urban, between(lat, 30, 60))
  mid_lat_no_urban <- rbind(mid_lat_no_urban, filter(biomes_no_urban, between(lat, -60, -30)))
  mid_lat_no_human <- filter(biomes_no_human, between(lat, 30, 60))
  mid_lat_no_human <- rbind(mid_lat_no_human, filter(biomes_no_human, between(lat, -60, -30)))

  mid_lat_m_all <- filter(megabiomes_all, between(lat, 30, 60))
  mid_lat_m_all <- rbind(mid_lat_m_all, filter(megabiomes_all, between(lat, -60, -30)))
  mid_lat_m_no_urban <- filter(megabiomes_no_urban, between(lat, 30, 60))
  mid_lat_m_no_urban <- rbind(mid_lat_m_no_urban, filter(megabiomes_no_urban, between(lat, -60, -30)))
  mid_lat_m_no_human <- filter(megabiomes_no_human, between(lat, 30, 60))
  mid_lat_m_no_human <- rbind(mid_lat_m_no_human, filter(megabiomes_no_human, between(lat, -60, -30)))

  low_lat_all <- filter(biomes_all, between(lat, -30, 30))
  low_lat_no_urban <- filter(biomes_no_urban, between(lat, -30, 30))
  low_lat_no_human <- filter(biomes_no_human, between(lat, -30, 30))

  low_lat_m_all <- filter(megabiomes_all, between(lat, -30, 30))
  low_lat_m_no_urban <- filter(megabiomes_no_urban, between(lat, -30, 30))
  low_lat_m_no_human <- filter(megabiomes_no_human, between(lat, -30, 30))

  #Calculate area for each latitude band (low versus high res)
  high_lat_area_lr <- sum(high_lat_all$area_km2)
  mid_lat_area_lr <- sum(mid_lat_all$area_km2)
  low_lat_area_lr <- sum(low_lat_all$area_km2)

  high_lat_area_hr <- sum(high_lat_no_urban$area_km2)
  mid_lat_area_hr <- sum(mid_lat_no_urban$area_km2)
  low_lat_area_hr <- sum(low_lat_no_urban$area_km2)

  #Remove NAs from no_human and no_urban
  high_lat_no_urban <- na.omit(high_lat_no_urban)
  high_lat_no_human <- na.omit(high_lat_no_human)
  high_lat_m_no_urban <- na.omit(high_lat_m_no_urban)
  high_lat_m_no_human <- na.omit(high_lat_m_no_human)

  mid_lat_no_urban <- na.omit(mid_lat_no_urban)
  mid_lat_no_human <- na.omit(mid_lat_no_human)
  mid_lat_m_no_urban <- na.omit(mid_lat_m_no_urban)
  mid_lat_m_no_human <- na.omit(mid_lat_m_no_human)

  low_lat_no_urban <- na.omit(low_lat_no_urban)
  low_lat_no_human <- na.omit(low_lat_no_human)
  low_lat_m_no_urban <- na.omit(low_lat_m_no_urban)
  low_lat_m_no_human <- na.omit(low_lat_m_no_human)

  #Count the biome changes between adjacent time slices
  all_high <- c(); all_mid <- c(); all_low <- c()
  no_urban_high <- c(); no_urban_mid <- c(); no_urban_low <- c()
  no_human_high <- c(); no_human_mid <- c(); no_human_low <- c()
  mega_all_high <- c(); mega_all_mid <- c(); mega_all_low <- c()
  mega_no_urban_high <- c(); mega_no_urban_mid <- c(); mega_no_urban_low <- c()
  mega_no_human_high <- c(); mega_no_human_mid <- c(); mega_no_human_low <- c()

  for (j in 1:(length(slices)-1)){
    all_high[j] <- sum(high_lat_all[which(high_lat_all[,4] != high_lat_all[,j+4]), 3]) / high_lat_area_lr
    no_urban_high[j] <- sum(high_lat_no_urban[which(high_lat_no_urban[,4] != high_lat_no_urban[,j+4]), 3]) / high_lat_area_hr
    no_human_high[j] <- sum(high_lat_no_human[which(high_lat_no_human[,4] != high_lat_no_human[,j+4]), 3]) / high_lat_area_hr
  
    all_mid[j] <- sum(mid_lat_all[which(mid_lat_all[,4] != mid_lat_all[,j+4]), 3]) / mid_lat_area_lr
    no_urban_mid[j] <- sum(mid_lat_no_urban[which(mid_lat_no_urban[,4] != mid_lat_no_urban[,j+4]), 3]) / mid_lat_area_hr
    no_human_mid[j] <- sum(mid_lat_no_human[which(mid_lat_no_human[,4] != mid_lat_no_human[,j+4]), 3]) / mid_lat_area_hr
  
    all_low[j] <- sum(low_lat_all[which(low_lat_all[,4] != low_lat_all[,j+4]), 3]) / low_lat_area_lr
    no_urban_low[j] <- sum(low_lat_no_urban[which(low_lat_no_urban[,4] != low_lat_no_urban[,j+4]), 3]) / low_lat_area_hr
    no_human_low[j] <- sum(low_lat_no_human[which(low_lat_no_human[,4] != low_lat_no_human[,j+4]), 3]) / low_lat_area_hr
  
    mega_all_high[j] <- sum(high_lat_m_all[which(high_lat_m_all[,4] != high_lat_m_all[,j+4]), 3]) / high_lat_area_lr
    mega_no_urban_high[j] <- sum(high_lat_m_no_urban[which(high_lat_m_no_urban[,4] != high_lat_m_no_urban[,j+4]), 3]) / high_lat_area_hr
    mega_no_human_high[j] <- sum(high_lat_m_no_human[which(high_lat_m_no_human[,4] != high_lat_m_no_human[,j+4]), 3]) / high_lat_area_hr
  
    mega_all_mid[j] <- sum(mid_lat_m_all[which(mid_lat_m_all[,4] != mid_lat_m_all[,j+4]), 3]) / mid_lat_area_lr
    mega_no_urban_mid[j] <- sum(mid_lat_m_no_urban[which(mid_lat_m_no_urban[,4] != mid_lat_m_no_urban[,j+4]), 3]) / mid_lat_area_hr
    mega_no_human_mid[j] <- sum(mid_lat_m_no_human[which(mid_lat_m_no_human[,4] != mid_lat_m_no_human[,j+4]), 3]) / mid_lat_area_hr
  
    mega_all_low[j] <- sum(low_lat_m_all[which(low_lat_m_all[,4] != low_lat_m_all[,j+4]), 3]) / low_lat_area_lr
    mega_no_urban_low[j] <- sum(low_lat_m_no_urban[which(low_lat_m_no_urban[,4] != low_lat_m_no_urban[,j+4]), 3]) / low_lat_area_hr
    mega_no_human_low[j] <- sum(low_lat_m_no_human[which(low_lat_m_no_human[,4] != low_lat_m_no_human[,j+4]), 3]) / low_lat_area_hr
  }

  #Add slice midpoints to plot against
  lat_plot <- as.data.frame(cbind(midpoints, all_high, all_mid, all_low,
                                  no_urban_high, no_urban_mid, no_urban_low,
                                  no_human_high, no_human_mid, no_human_low))
  lat_plot <- pivot_longer(lat_plot, !midpoints, names_to = "latitude")
  lat_plot$RCP <- RCPs[m]
  lat_plot$footprint <- sub('_[^_]*$', '', lat_plot$latitude)
  lat_plot$latitude <- sub(".*_", "", lat_plot$latitude)
  lat_plot_all <- rbind (lat_plot_all, lat_plot)
  
  lat_plot_m <- as.data.frame(cbind(midpoints, mega_all_high, mega_all_mid, mega_all_low,
                                    mega_no_urban_high, mega_no_urban_mid, mega_no_urban_low,
                                    mega_no_human_high, mega_no_human_mid, mega_no_human_low))
  lat_plot_m <- pivot_longer(lat_plot_m, !midpoints, names_to = "latitude",
                             names_prefix = "mega_")
  lat_plot_m$RCP <- RCPs[m]
  lat_plot_m$footprint <- sub('_[^_]*$', '', lat_plot_m$latitude)
  lat_plot_m$latitude <- sub(".*_", "", lat_plot_m$latitude)
  mega_lat_plot_all <- rbind(mega_lat_plot_all, lat_plot_m)
  
  #Track progress
  print(m)
}

#Plot
ggplot(data = lat_plot_all, aes(x = midpoints, y = value,
                                group = footprint, colour = footprint)) +
  geom_line() +
  facet_grid(RCP ~ latitude) +
  scale_colour_manual(values = c("firebrick1", "cadetblue1", "green2")) +
  xlab("Year") + ylab("Proportion of terrestrial area undergoing biome transitions between slices") +
  theme_classic()
ggsave(paste0("figures/Biome transitions from present by latitude.pdf"), width = 10, height = 6, dpi = 600)

ggplot(data = mega_lat_plot_all, aes(x = midpoints, y = value,
                                     group = footprint, colour = footprint)) +
  geom_line() +
  facet_grid(RCP ~ latitude) +
  scale_colour_manual(values = c("firebrick1", "cadetblue1", "green2")) +
  xlab("Year") + ylab("Proportion of terrestrial area undergoing megabiome transitions between slices") +
  theme_classic()
ggsave(paste0("figures/Megabiome transitions from present by latitude.pdf"), width = 10, height = 6, dpi = 600)
