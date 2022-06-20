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
  biome_vector <- as.vector(biomes)

  #Pull latitude and longitude values
  if (i == 1){lon <- ncvar_get(netCDF, "lon")
  lat <- ncvar_get(netCDF, "lat")
  lonlat <- as.matrix(expand.grid(lon,lat))
  biome_table <- data.frame(lonlat)
  names(biome_table) <- c("lon","lat")}
  
  #Add slice to matrix
  biome_table[,(i + 2)] <- biome_vector
  colnames(biome_table)[(i + 2)] <- slices[i]
}

#Write table, removing NA values
biome_table <- na.omit(biome_table)
write.csv(biome_table, "data/cleaned/RCP6.csv", row.names = F)

#Read in biome conversion
conversion <- read.table("data/biome_conversion.txt", sep = ",")

#Convert biomes into megabiomes
megabiome_table <- data.frame()
megabiome_table <- as.data.frame(lapply(biome_table, function(x) conversion$V3[match(x, conversion$V1)]))
megabiome_table[,1:2] <- biome_table[,1:2]

#Write table
write.csv(megabiome_table, "data/cleaned/RCP6_mega.csv", row.names = F)
