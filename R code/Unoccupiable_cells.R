#Bethany Allen   6th April 2023
#Code to calculate the number of cells without same/adjacent cells of the
# same biome in the previous time slice (making occupation difficult)

library(tidyverse)

#List filenames
filenames <- c("RCP2.6_all", "RCP2.6_no_urban", "RCP2.6_no_human",
               "RCP4.5_all", "RCP4.5_no_urban", "RCP4.5_no_human",
               "RCP6_all", "RCP6_no_urban", "RCP6_no_human")

midpoints <- seq(from = 2020, to = 2480, by = 20)

#Read in biome conversion
#conversion <- read.table("data/biome_conversion.txt", sep = ",")

count_table <- data.frame()

for (l in 1:length(filenames)) {
  #Read file
  biomes <- read.csv(paste0("data/cleaned/", filenames[l], ".csv"))
  
  #Remove NAs (needed for no_urban and no_human)
  biomes <- na.omit(biomes)
  
  #For each pair of adjacent slices, loop through individual biomes
  #For each cell, test for the same biome adjacent in previous slice
  
  for (k in 1:24) {
    slice1 <- biomes[,c(1, 2, (3 + k))]
    slice2 <- biomes[,c(1, 2, (4 + k))]
    
    for (i in 1:28) {
      #Filter to cells of a single biome
      biome_cells1 <- slice1[which(slice1[,3] == i),]
      biome_cells2 <- slice2[which(slice2[,3] == i),]
    
      cell_count <- 0
    
      if (nrow(biome_cells2) > 0) {
        
        #For each cell in the second slice
        for (j in 1:nrow(biome_cells2)) {
          
          #Select lat/long
          cell_lon <- biome_cells2[j,1]
          cell_lat <- biome_cells2[j,2]
      
          #If cell wasn't occupied in previous slice, check neighbours
          if (nrow(filter(biome_cells1, lon == cell_lon,
                          lat == cell_lat)) != 1) {
            
            #Extend into 9 by 9 cell grid
            if (l %in% c(1, 4, 7)) {
              cell_upper_lon <- cell_lon + 0.5
              cell_lower_lon <- cell_lon - 0.5
              cell_upper_lat <- cell_lat + 0.5
              cell_lower_lat <- cell_lat - 0.5
            } else {
              cell_upper_lon <- cell_lon + 0.0833
              cell_lower_lon <- cell_lon - 0.0833
              cell_upper_lat <- cell_lat + 0.0833
              cell_lower_lat <- cell_lat - 0.0833
            }
      
            #Wrap around limits if needed
            if (cell_upper_lon > 180) {cell_upper_lon <- cell_upper_lon - 360}
            if (cell_lower_lon < -180) {cell_lower_lon <- cell_lower_lon + 360}
      
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
      }
    #Add to table of counts
    count_table <- rbind(count_table, c(midpoints[k], i, filenames[l],
                                        cell_count, nrow(biome_cells2)))
    
    #Print progress
    print(paste("File", l, "Slice", k, "Biome", i))
    }
  }
  #Add labels
  colnames(count_table) <- c("Year", "Biome", "File", "Unoccupiable_cells",
                             "Total_cells")
  
  #Add proportions
  count_table$Proportion <- as.numeric(count_table$Unoccupiable_cells) /
    as.numeric(count_table$Total_cells)
  
  #Save table
  write.csv(count_table, paste0("data/counts/", filenames[l], ".csv"),
            row.names = FALSE)
}

#Read in table
count_table <- read.csv("data/counts/RCP6_all.csv")

count_table$Biome <- as.factor(count_table$Biome)

#Plot
ggplot(data = count_table, aes(x = Year, y = Proportion, group = Biome,
                               colour = Biome)) +
  geom_line() +
  xlab("Year") + ylab("Proportion of biome cells not adjacent to those in previous time slice") +
  scale_x_continuous(guide = guide_axis(angle = 90)) +
  theme_classic()
ggsave("figures/Unoccupiable_cells.pdf", width = 10, height = 6, dpi = 600)
