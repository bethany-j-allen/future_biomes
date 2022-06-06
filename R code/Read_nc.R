#Bethany Allen   6th June 2022
#Code to extract relevant information from netCDF files

library(ncdf4)

slices <- c("2000-2019", "2020-2039", "2040-2059", "2060-2079", "2080-2099",
            "2100-2119", "2120-2139", "2140-2159", "2160-2179", "2180-2199",
            "2200-2219", "2220-2239", "2240-2259", "2260-2279", "2280-2299",
            "2300-2319", "2320-2339", "2340-2359", "2360-2379", "2380-2399",
            "2400-2419", "2420-2439", "2440-2459", "2460-2479", "2480-2499")

for (i in 1:length(slices)){
  #Create filename
  filename <- paste0("data/netCDFs/xoazm_", slices[i], "AD_biome4out.nc")
  
  #Read in netCDF
  netCDF <- nc_open(filename)
  #print(netCDF)

  #Pull biome grid
  biomes <- ncvar_get(netCDF, "biome")

  #Pull latitude and longitude values
  lon <- ncvar_get(netCDF, "lon")
  lat <- ncvar_get(netCDF, "lat")

  #Create table of grid squares
  lonlat <- as.matrix(expand.grid(lon,lat))

  #Add biome values to data frame
  biome_vector <- as.vector(biomes)
  biome_table <- data.frame(cbind(lonlat,biome_vector))
  names(biome_table) <- c("lon","lat","biome")

  #Create export filename
  to_write <- paste0("data/cleaned/", slices[i], ".csv")
  
  #Write table, removing NA values
  write.csv(na.omit(biome_table), to_write, row.names = F)
}
