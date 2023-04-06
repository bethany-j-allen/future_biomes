#Bethany Allen   6th April 2023
#Code to calculate the number of cells without same/adjacent cells of the
# same biome in the previous time slice (making occupation difficult)

library(tidyverse)
library(raster)

#List columns
slices <- c("2000-2019", "2020-2039", "2040-2059", "2060-2079", "2080-2099",
            "2100-2119", "2120-2139", "2140-2159", "2160-2179", "2180-2199",
            "2200-2219", "2220-2239", "2240-2259", "2260-2279", "2280-2299",
            "2300-2319", "2320-2339", "2340-2359", "2360-2379", "2380-2399",
            "2400-2419", "2420-2439", "2440-2459", "2460-2479", "2480-2499")
midpoints <- seq(from = 2010, to = 2490, by = 20)
megabiome_tags <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")

#Read in biome conversion
conversion <- read.table("data/biome_conversion.txt", sep = ",")

count_table <- data.frame()

for (k in 1:(length(slices) - 1)){
  #Create filename
  filename1 <- paste0("data/netCDFs/RCP6/xoazm_", slices[k], "AD_biome4out.nc")
  filename2 <- paste0("data/netCDFs/RCP6/xoazm_", slices[(k + 1)], "AD_biome4out.nc")
  
  #Read in netCDFs
  slice1 <- raster(filename1, varname = "biome")
  slice2 <- raster(filename2, varname = "biome")
  
  #Rotate longitude values from 0 to 360 to -180 to 180
  slice1 <- rotate(slice1)
  slice2 <- rotate(slice2)
  
  #Convert raster into data frame
  slice1_table <- data.frame(coordinates(slice1), values(slice1))
  slice2_table <- data.frame(coordinates(slice2), values(slice2))
  names(slice1_table) <- c("lon", "lat", "biome")
  names(slice2_table) <- c("lon", "lat", "biome")
  
  #Remove NAs
  slice1_table <- na.omit(slice1_table)
  slice2_table <- na.omit(slice2_table)
  
  #Filter slices to a single biome, and determine whether any of the
  # neighbouring cells were occupied in the previous time slice
  
  for (i in 1:28) {
    #Filter to cells of a single biome
    biome_cells1 <- slice1_table[which(slice1_table[,3] == i),]
    biome_cells2 <- slice2_table[which(slice2_table[,3] == i),]
    
    cell_count <- 0
    
    if (nrow(biome_cells2) > 0) {
    
      #For each cell in the second slice
      for (j in 1:nrow(biome_cells2)){
        #Select lat/long
        cell_lon <- biome_cells2[j,1]
        cell_lat <- biome_cells2[j,2]
      
        #Extend into 9 by 9 cell grid
        cell_upper_lon <- cell_lon + 0.5
        cell_lower_lon <- cell_lon - 0.5
        cell_upper_lat <- cell_lat + 0.5
        cell_lower_lat <- cell_lat - 0.5
      
        #Wrap around limits if needed
        if (cell_upper_lon > 180) {cell_upper_lon <- cell_upper_lon - 360}
        if (cell_lower_lon < -180) {cell_lower_lon <- cell_lower_lon + 360}
        if (cell_upper_lat > 90) {cell_upper_lon <- cell_upper_lon - 180}
        if (cell_lower_lon < -90) {cell_lower_lon <- cell_lower_lon + 180}
      
        #Filter first time slice to 9 by 9 grid
        test_area <- filter(biome_cells1, lon %in% c(cell_upper_lon,
                                                cell_lon,
                                                cell_lower_lon))
        test_area <- filter(test_area, lat %in% c(cell_upper_lat,
                                                  cell_lat,
                                                  cell_lower_lat))
        #If no cells in grid are found, add to tally
        if (nrow(test_area) == 0) {cell_count <- cell_count + 1}
      }
    }
    #Add to table of counts
    count_table <- rbind(count_table, c(midpoints[k], i, cell_count, nrow(biome_cells2)))
    
    #Print progress
    print(paste("Slice", k, "Biome", i))
  }
}

#Add labels
colnames(count_table) <- c("Year", "Biome", "Unoccupiable_cells", "Total_cells")
