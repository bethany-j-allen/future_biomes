#Bethany Allen   6th June 2022
#Code to extract relevant information from netCDF files and calculate cell area

library(raster)
library(units)

slices <- c("2000-2019", "2020-2039", "2040-2059", "2060-2079", "2080-2099",
            "2100-2119", "2120-2139", "2140-2159", "2160-2179", "2180-2199",
            "2200-2219", "2220-2239", "2240-2259", "2260-2279", "2280-2299",
            "2300-2319", "2320-2339", "2340-2359", "2360-2379", "2380-2399",
            "2400-2419", "2420-2439", "2440-2459", "2460-2479", "2480-2499")

for (i in 1:length(slices)){
  #Create filename
  filename <- paste0("data/netCDFs/RCP6/xoazm_", slices[i], "AD_biome4out.nc")

  #Read in netCDF
  netCDF <- raster(filename, varname = "biome")
  #print(netCDF)
  
  #Rotate longitude values from 0 to 360 to -180 to 180
  netCDF <- rotate(netCDF)

  #Start table with latitude and longitude values
  if (i == 1){
    biome_table <- data.frame(coordinates(netCDF))
    names(biome_table) <- c("lon", "lat")
    
    #Calculate cell areas
    #Convert to cell limits
    biome_table$lowerlon <- biome_table$lon - 0.25
    biome_table$upperlon <- biome_table$lon + 0.25
    biome_table$lowerlat <- biome_table$lat - 0.25
    biome_table$upperlat <- biome_table$lat + 0.25
    
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
    biome_table$lowerlon <- as_units(biome_table$lowerlon, "degrees")
    biome_table$lowerlon <- set_units(biome_table$lowerlon, "radians")
    
    biome_table$upperlon <- as_units(biome_table$upperlon, "degrees")
    biome_table$upperlon <- set_units(biome_table$upperlon, "radians")
    
    biome_table$lowerlat <- as_units(biome_table$lowerlat, "degrees")
    biome_table$lowerlat <- set_units(biome_table$lowerlat, "radians")
    
    biome_table$upperlat <- as_units(biome_table$upperlat, "degrees")
    biome_table$upperlat <- set_units(biome_table$upperlat, "radians")
    
    #Calculate area in square metres
    biome_table$area_km2 = (R^2) * (biome_table$upperlon - biome_table$lowerlon) *
      (sin(biome_table$upperlat) - sin(biome_table$lowerlat))
    
    #Remove units and convert to square kilometres
    units(biome_table$area_km2) <- NULL
    biome_table$area_km2 <- biome_table$area_km2 / 1000000
    
    #Remove perimeter columns
    biome_table <- biome_table[,c("lon", "lat", "area_km2")]
  }

  #Add biome data for time slice to matrix
  biome_table[,(i + 3)] <- raster::values(netCDF)
  colnames(biome_table)[(i + 3)] <- slices[i]
}

#Remove NA values
biome_table <- na.omit(biome_table)

#Save table
write.csv(biome_table, "data/cleaned/RCP6_all.csv", row.names = F)

#Read in biome conversion
conversion <- read.table("data/biome_conversion.txt", sep = ",")

#Convert biomes into megabiomes
megabiome_table <- data.frame()
megabiome_table <- as.data.frame(lapply(biome_table, function(x) conversion$V3[match(x, conversion$V1)]))
megabiome_table[,1:3] <- biome_table[,1:3]

#Write table
write.csv(megabiome_table, "data/cleaned/RCP6_mega_all.csv", row.names = F)
