#Bethany Allen   6th June 2022
#Code to extract relevant information from netCDF files

library(ncdf4)

#Read in netCDF
netCDF <- nc_open("data/netCDFs/xoazm_2000-2019AD_biome4out.nc")
print(netCDF)

#Pull biome grid
biomes <- ncvar_get(netCDF, "biome")

#Pull latitude and longitude values
lon <- ncvar_get(test, "lon")
lat <- ncvar_get(test, "lat")

#Create table of grid squares
lonlat <- as.matrix(expand.grid(lon,lat))

#Add biome values to data frame
biome_vector <- as.vector(biomes)
biome_table <- data.frame(cbind(lonlat,biome_vector))
names(biome_table) <- c("lon","lat","biome")

#Write table, removing NA values
write.csv(na.omit(biome_table), "data/cleaned/2010.csv", row.names = F)
