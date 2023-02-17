#Bethany Allen   17th Feb 2023
#Code to extract anthropogenic activity (HYDE anthromes) from biome maps

library(tidyverse)
library(raster)

#Load HYDE 3.2 anthromes for 2017
anthromes_raster <- raster("data/anthromes2017AD.asc")

#Extract values from raster
anthromes_df <- as.data.frame(cbind(coordinates(anthromes_raster),
                             value = raster::values(anthromes_raster)))
head(anthromes_df)

#Reomve NAs
anthromes_df <- filter(anthromes_df, !is.na(value))

#Split into urban and all human regions
urban_area <- filter(anthromes_df, value == 11 | value == 12)
human_codes <- c(11, 12, 21, 22, 23, 24, 31, 32, 33, 34, 41, 42, 43)
human_area <- filter(anthromes_df, value %in% human_codes)
