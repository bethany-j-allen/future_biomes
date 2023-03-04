#Bethany Allen   17th Feb 2023
#Code to extract anthropogenic activity (HYDE anthromes) from biome maps

library(tidyverse)
library(raster)

#Load HYDE 3.2 anthromes for 2017
anthromes_raster <- raster("data/anthromes2017AD.asc")

#Load biome raster
biomes <- raster("data/netCDFs/RCP6/xoazm_2000-2019AD_biome4out.nc",
                 varname = "biome")

#Rotate longitude values from 0 to 360 to -180 to 180
biomes <- rotate(biomes)

#Split biome raster into cells of same size as anthromes
smaller_cells <- disaggregate(biomes, fact = c(6, 6))

#Resize anthrome raster to have same extent as biomes
anthromes_resized <- setExtent(anthromes_raster, smaller_cells, snap = TRUE)

#Extract values from both rasters
anthromes_df <- as.data.frame(cbind(coordinates(anthromes_resized),
                                    value = raster::values(anthromes_resized)))
head(anthromes_df)
biomes_df <- as.data.frame(cbind(coordinates(smaller_cells),
                                    value = raster::values(smaller_cells)))
head(biomes_df)

#Remove NAs
anthromes_df <- filter(anthromes_df, !is.na(value))
biomes_df <- filter(biomes_df, !is.na(value))

#Swivel round longitude values on biome dataframe
biomes_df$x <- ifelse((biomes_df$x > 180), (biomes_df$x - 360), biomes_df$x)

#Combine datasets
combined_df <- left_join(anthromes_df, biomes_df, by = c("x", "y"),
                  suffix = c("anthrome", "biome"))

test <- filter(combined_df, is.na(valuebiome))


#Split into urban and all human regions
urban_area <- filter(anthromes_df, value == 11 | value == 12)
human_codes <- c(11, 12, 21, 22, 23, 24, 31, 32, 33, 34, 41, 42, 43)
human_area <- filter(anthromes_df, value %in% human_codes)
