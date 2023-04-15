#Bethany Allen   17th Feb 2023
#Code to remove anthropogenic activity (HYDE anthromes) from biome maps

library(tidyverse)
library(raster)
library(units)

slices <- c("2000-2019", "2020-2039", "2040-2059", "2060-2079", "2080-2099",
            "2100-2119", "2120-2139", "2140-2159", "2160-2179", "2180-2199",
            "2200-2219", "2220-2239", "2240-2259", "2260-2279", "2280-2299",
            "2300-2319", "2320-2339", "2340-2359", "2360-2379", "2380-2399",
            "2400-2419", "2420-2439", "2440-2459", "2460-2479", "2480-2499")

#Load HYDE 3.2 anthrome map for 2017
anthromes_raster <- raster("data/anthromes2017AD.asc")

#Resize anthrome raster to have same extent as biomes
anthromes_resized <- setExtent(anthromes_raster, c(-180, 180, -90, 90))

#Extract values from anthrome raster and remove NAs
anthromes_df <- as.data.frame(cbind(coordinates(anthromes_resized),
                                    value = raster::values(anthromes_resized)))
anthromes_df <- filter(anthromes_df, !is.na(value))

for (i in 1:length(slices)){
  #Create filename
  filename <- paste0("data/netCDFs/RCP6/xoazm_", slices[i], "AD_biome4out.nc")
  
  #Read in netCDF
  biomes <- raster(filename, varname = "biome")

  #Rotate longitude values from 0 to 360 to -180 to 180
  biomes <- rotate(biomes)

  #Split biome raster into cells of same size as anthromes
  smaller_cells <- disaggregate(biomes, fact = c(6, 6))

  #Extract biome values and remove NAs
  biomes_df <- as.data.frame(cbind(coordinates(smaller_cells),
                                     value = raster::values(smaller_cells)))
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
    combined_df$area_km2 = (R^2) *
      (combined_df$upperlon -combined_df$lowerlon) *
      (sin(combined_df$upperlat) - sin(combined_df$lowerlat))
  
    #Remove units and convert to square kilometres
    units(combined_df$area_km2) <- NULL
    combined_df$area_km2 <- combined_df$area_km2 / 1000000
  
    #Remove perimeter columns
    combined_df <- dplyr::select(combined_df, lon, lat, anthrome, biome,
                                 area_km2)
  }

  #Create datasets excluding urban areas, and excluding all human activity
  no_urban <- filter(combined_df,
                       anthrome != 11 & anthrome != 12)
  no_urban <- dplyr::select(no_urban, lon, lat, biome)
  non_human_codes <- c(51, 52, 53, 54, 61, 62, 63)
  no_humans <- filter(combined_df, anthrome %in% non_human_codes)
  no_humans <- dplyr::select(no_humans, lon, lat, biome)

  #For first slice, create overall dataframe with cell areas
  if (i == 1){
    no_urban_df <- dplyr::select(combined_df, lon, lat, area_km2)
    no_human_df <- dplyr::select(combined_df, lon, lat, area_km2)
  }

  no_urban_df <- left_join(no_urban_df, no_urban, by = c("lon", "lat"))
  colnames(no_urban_df)[i + 3] <- slices[i]
  no_human_df <- left_join(no_human_df, no_humans, by = c("lon", "lat"))
  colnames(no_human_df)[i + 3] <- slices[i]
}

#Save table
no_urban_df$lon <- round(no_urban_df$lon, 4)
no_urban_df$lat <- round(no_urban_df$lat, 4)
no_human_df$lon <- round(no_human_df$lon, 4)
no_human_df$lon <- round(no_human_df$lon, 4)
write.csv(no_urban_df, "data/cleaned/RCP6_no_urban.csv", row.names = F)
write.csv(no_human_df, "data/cleaned/RCP6_no_human.csv", row.names = F)

#Read in biome conversion
conversion <- read.table("data/biome_conversion.txt", sep = ",")

#Convert biomes into megabiomes
mega_urban_table <- data.frame()
mega_human_table <- data.frame()
mega_urban_table <- as.data.frame(lapply(no_urban_df, function(x) conversion$V3[match(x, conversion$V1)]))
mega_human_table <- as.data.frame(lapply(no_human_df, function(x) conversion$V3[match(x, conversion$V1)]))
mega_urban_table[,1:3] <- no_urban_df[,1:3]
mega_human_table[,1:3] <- no_human_df[,1:3]

#Write table
write.csv(mega_urban_table, "data/cleaned/RCP6_mega_no_urban.csv", row.names = F)
write.csv(mega_human_table, "data/cleaned/RCP6_mega_no_human.csv", row.names = F)
