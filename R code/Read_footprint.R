#Bethany Allen   17th Feb 2023
#Code to extract anthropogenic activity (HYDE anthromes) from biome maps

library(tidyverse)
library(raster)
library(units)

#Load HYDE 3.2 anthrome map for 2017
anthromes_raster <- raster("data/anthromes2017AD.asc")

#Load biome raster
biomes <- raster("data/netCDFs/RCP6/xoazm_2000-2019AD_biome4out.nc",
                 varname = "biome")

#Rotate longitude values from 0 to 360 to -180 to 180
biomes <- rotate(biomes)

#Split biome raster into cells of same size as anthromes
smaller_cells <- disaggregate(biomes, fact = c(6, 6))

#Resize anthrome raster to have same extent as biomes
anthromes_resized <- setExtent(anthromes_raster, c(-180, 180, -90, 90))

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

#Combine datasets
combined_df <- left_join(anthromes_df, biomes_df, by = c("x", "y"),
                  suffix = c("anthrome", "biome"))
combined_df <- filter(combined_df, !is.na(valuebiome))
names(combined_df) <- c("lon", "lat", "anthrome", "biome")

#Remove "no definition" anthrome cells
combined_df <- filter(combined_df, anthrome != 70)

if (i == 1){
  #Calculate cell areas
  #Convert to cell limits
  combined_df$lowerlon <- combined_df$lon - (0.5/12)
  combined_df$upperlon <- combined_df$lon + (0.5/12)
  combined_df$lowerlat <- combined_df$lat - (0.5/12)
  combined_df$upperlat <- combined_df$lat + (0.5/12)
  
  #Code uses the equation of Santini et al. (2010)
  # S = R^2(λ2-λ1)(sinφ2-sinφ1)
  # where
  # S = surface area of a latitude-longitude grid cell
  # R = radius of the Earth
  # λ1, λ2 = longitude bounds of the cell, in radians
  # φ1, φ2 = latitude bounds of the cell, in radians
  
  #Mean radius of the Earth
  R = 6371008.8
  
  #Convert edges to radians
  combined_df$lowerlon <- as_units(combined_df$lowerlon, "degrees")
  combined_df$lowerlon <- set_units(combined_df$lowerlon, "radians")
  
  combined_df$upperlon <- as_units(combined_df$upperlon, "degrees")
  combined_df$upperlon <- set_units(combined_df$upperlon, "radians")
  
  combined_df$lowerlat <- as_units(combined_df$lowerlat, "degrees")
  combined_df$lowerlat <- set_units(combined_df$lowerlat, "radians")
  
  combined_df$upperlat <- as_units(combined_df$upperlat, "degrees")
  combined_df$upperlat <- set_units(combined_df$upperlat, "radians")
  
  #Calculate area in square metres
  combined_df$area_km2 = (R^2) * (combined_df$upperlon - combined_df$lowerlon) *
    (sin(combined_df$upperlat) - sin(combined_df$lowerlat))
  
  #Remove units and convert to square kilometres
  units(combined_df$area_km2) <- NULL
  combined_df$area_km2 <- combined_df$area_km2 / 1000000
  
  #Remove perimeter columns
  combined_df <- combined_df[,c("lon", "lat", "anthrome", "biome",
  "area_km2")]
}

#Create datasets excluding urban areas, and excluding all human activity
no_urban <- filter(combined_df,
                     valueanthrome != 11 & valueanthrome != 12)
non_human_codes <- c(51, 52, 53, 54, 61, 62, 63)
no_humans <- filter(combined_df, valueanthrome %in% non_human_codes)

#For first slice, create overall dataframe with cell areas
if (i == 1){
  no_urban_df <- combined_df[,c("lon", "lat", "area_km2")]
  no_human_df <- combined_df[,c("lon", "lat", "area_km2")]
}

#Figure out how to join filtered columns onto dataset

