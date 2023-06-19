#Bethany Allen   21st Dec 2022
#Code to count patches and quantify fragmentation of biomes

library(tidyverse)
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
RCPs <- c("RCP2.6/xoazl", "RCP4.5/xoazk", "RCP6/xoazm")

to_plot <- data.frame()

for (k in 1:length(RCPs)){
  patch_table <- data.frame()
  for (i in 1:length(slices)){
    
    #Create filename
    filename <- paste0("data/netCDFs/", RCPs[k], "_", slices[i], "AD_biome4out.nc")
  
    #Read in netCDF
    biomes <- raster(filename, varname = "biome")
  
    #Delineate patches
    patches <- get_patches(biomes, directions = 8)
    patches <- patches[[1]]
    names(patches) <- gsub("class_", "", names(patches))
  
    for (j in 1:26){
      if (j %in% names(patches)){
        patch_count <- length(raster::unique(patches[[paste0(j)]]))
        patch_table[j,i] <- patch_count } else
          { patch_table[j,i] <- 0 }
    }
  }

  #Clean table
  colnames(patch_table) <- midpoints
  patch_table$biomes <- seq(1, 26, 1)
  patch_table$labels <- conversion$V3[1:26]
  patch_table <- pivot_longer(patch_table, !c(biomes, labels))
  patch_table$RCP <- RCPs[k]
  to_plot <- rbind(to_plot, patch_table)
}

to_plot$RCP <- gsub('/.*', '', to_plot$RCP)
to_plot$labels <- factor(to_plot$labels,
                              levels = conversion$V3[1:26])
to_plot$name <- as.numeric(to_plot$name)

#Plot
ggplot(data = to_plot, aes(x = name, y = value,
                                group = labels, col = labels)) +
  geom_line() +
  facet_wrap( ~ RCP, ncol = 3) +
  xlab("Year") + ylab("Number of patches") +
  scale_x_continuous(guide = guide_axis(angle = 90)) +
  theme_classic()
ggsave("figures/Patch_counts.pdf", width = 10, height = 6, dpi = 600)
