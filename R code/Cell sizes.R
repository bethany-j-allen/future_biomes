#Bethany Allen   25th Jan 2023
#Code to calculate the areas of each grid cell

library(tidyverse)
library(ncdf4)
library(raster)
library(units)

#Read in one netCDF
filename <- paste0("data/netCDFs/RCP6/xoazm_2000-2019AD_biome4out.nc")

#Read in netCDF
netCDF <- nc_open(filename)

#Start table with latitude and longitude values, and calculate cell area
lon <- ncvar_get(netCDF, "lon")
lat <- ncvar_get(netCDF, "lat")
lonlat <- as.data.frame(expand.grid(lon,lat))
colnames(lonlat) <- c("lon", "lat")

#Convert to cell limits
lonlat$lowerlon <- lonlat$lon - 0.25
lonlat$upperlon <- lonlat$lon + 0.25
lonlat$lowerlat <- lonlat$lat - 0.25
lonlat$upperlat <- lonlat$lat + 0.25

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
lonlat$lowerlon <- as_units(lonlat$lowerlon, "degrees")
lonlat$lowerlon <- set_units(lonlat$lowerlon, "radians")

lonlat$upperlon <- as_units(lonlat$upperlon, "degrees")
lonlat$upperlon <- set_units(lonlat$upperlon, "radians")

lonlat$lowerlat <- as_units(lonlat$lowerlat, "degrees")
lonlat$lowerlat <- set_units(lonlat$lowerlat, "radians")

lonlat$upperlat <- as_units(lonlat$upperlat, "degrees")
lonlat$upperlat <- set_units(lonlat$upperlat, "radians")

#Calculate area in square metres
lonlat$area = (R^2) * (lonlat$upperlon - lonlat$lowerlon) *
  (sin(lonlat$upperlat) - sin(lonlat$lowerlat))

#Remove units and convert to square kilometres
units(lonlat$area_km2) <- NULL
lonlat$area_km2 <- lonlat$area_km2 / 1000000

#Paste onto data tables
biomes <- read.csv("data/cleaned/RCP6.csv")
megabiomes <- read.csv("data/cleaned/RCP6_mega.csv")

lonlat <- select(lonlat, lon, lat, area_km2)
biomes <- right_join(lonlat, biomes, by = c("lon", "lat"), keep = F)
megabiomes <- right_join(lonlat, megabiomes, by = c("lon", "lat"), keep = F)

#Write new table
write.csv(biome_table, "data/cleaned/RCP6.csv", row.names = F)
write.csv(megabiome_table, "data/cleaned/RCP6_mega.csv", row.names = F)
