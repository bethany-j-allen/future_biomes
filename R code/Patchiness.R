#Bethany Allen   21st Dec 2022
#Code to count patches and quantify fragmentation of biomes

library(tidyverse)
library(ncdf4)
library(raster)
library(landscapemetrics)

#Read in biome conversion
conversion <- read.table("data/biome_conversion.txt", sep = ",")

#List columns
slices <- c("2000-2019", "2020-2039", "2040-2059", "2060-2079", "2080-2099",
            "2100-2119", "2120-2139", "2140-2159", "2160-2179", "2180-2199",
            "2200-2219", "2220-2239", "2240-2259", "2260-2279", "2280-2299",
            "2300-2319", "2320-2339", "2340-2359", "2360-2379", "2380-2399",
            "2400-2419", "2420-2439", "2440-2459", "2460-2479", "2480-2499")
midpoints <- seq(from = 2010, to = 2490, by = 20)
megabiome_tags <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")

patch_table <- data.frame()

for (i in 1:length(slices)){
  #Create filename
  filename <- paste0("data/netCDFs/RCP6/xoazm_", slices[i], "AD_biome4out.nc")
  
  #Read in netCDF
  netCDF <- nc_open(filename)
  #print(netCDF)
  
  #Pull biome grid as a raster
  biomes <- ncvar_get(netCDF, "biome")
  biome_raster <- raster(biomes)
  extent(biome_raster) <- c(0, 360, -90, 90)
  patches <- get_patches(biome_raster, directions = 8)
  patches <- patches[[1]]
  names(patches) <- gsub("class_", "", names(patches))
  
  for (j in 1:28){
    if (j %in% names(patches)){
      patch_count <- length(raster::unique(patches[[j]]))
      patch_table[j,i] <- patch_count } else { patch_table[j,i] <- 0 }
  }

}

colnames(patch_count) <- slices
rownames(patch_count) <- c(seq(1, 17, 1), seq(19, 28, 1))
