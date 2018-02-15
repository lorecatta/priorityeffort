rm(list = ls())

# load packages
library(ggplot2)
library(rgdal)
library(dplyr)
library(RColorBrewer)
library(rgeos)

# load data
daly_subcatch <- readOGR(dsn = file.path("data", "shapefiles"), layer = "Catch_500")
site_species_array_df <- read.csv (file.path("data", "input_files","Planning Unit vs. Conservation Feature.csv"), header=TRUE, sep = ",")
cons_feat_array_df <- read.csv (file.path("data","input_files","Conservation Feature.csv"), header=TRUE, sep = ",")

# dissolve subcatchment layer to get region outline
daly_subcatchment_layer_outline <- gUnaryUnion(daly_subcatch, id = daly_subcatch@data$OID_)

cons_feat_array_df$sp_names <- names(site_species_array_df)
fish_spp <- cons_feat_array_df[cons_feat_array_df$FaunalGroup==1,"sp_names"]
turtle_spp <- cons_feat_array_df[cons_feat_array_df$FaunalGroup==2,"sp_names"]
birds_spp <- cons_feat_array_df[cons_feat_array_df$FaunalGroup==3,"sp_names"]

# Also...
tot_area_occ <- colSums(site_species_array_df)
tot_area_occ <- tot_area_occ[order(tot_area_occ)]
top_20_rarest <- tot_area_occ[1:20] 

site_species_array_df$GridID <- seq_len(nrow(site_species_array_df))

# join site action array to the shapefile 
daly_subcatch@data <- left_join(daly_subcatch@data, site_species_array_df, by = 'GridID')

YlOrRd <- brewer.pal(9, "YlOrRd")[c(2,5,7,9)]
color.palette <- colorRampPalette(YlOrRd) # this is a function!

# OR also very easy
#library("colorspace")
#pal <- choose_palette()

daly_subcatchment_layer_outline_list <- list("sp.polygons", 
                                             daly_subcatchment_layer_outline, 
                                             col = "black", 
                                             first = FALSE)

# to remove 0 values from plotting 
daly_subcatch@data[daly_subcatch@data==0] <- NA

tiff(filename = file.path("figures", paste("fish_species_distribution", "tiff", sep=".")), 
     width = 10, height = 5, units = "in", compression = "lzw", res = 200)
print(spplot(daly_subcatch, fish_spp, 
             col = NA, 
             layout = c(11,4),
             as.table = TRUE,
             main = list(label = "Area of occupancy (fishes)", cex = 1.3),
             col.regions = color.palette(20),
             sp.layout = list(daly_subcatchment_layer_outline_list)))
dev.off()

tiff(filename = file.path("figures", paste("turtle_species_distribution", "tiff", sep=".")), 
     width = 7, height = 4, units = "in", compression = "lzw", res = 200)
print(spplot(daly_subcatch, turtle_spp, 
             col = NA, 
             layout = c(4,2),
             as.table = TRUE,
             main = list(label = "Area of occupancy (turtles)", cex = 1.3),
             col.regions = color.palette(20),
             sp.layout = list(daly_subcatchment_layer_outline_list)))
dev.off()

tiff(filename = file.path("figures", paste("bird_species_distribution", "tiff", sep=".")), 
     width = 11, height = 11, units = "in", compression = "lzw", res = 200)
print(spplot(daly_subcatch, birds_spp, 
             col = NA, 
             layout = c(10,9),
             as.table = TRUE,
             main = list(label = "Area of occupancy (waterbirds)", cex = 1.3),
             col.regions = color.palette(20),
             sp.layout = list(daly_subcatchment_layer_outline_list)))
dev.off()

tiff(filename = file.path("figures", paste("top_20_rarest_species_distribution", "tiff", sep=".")), 
     width = 7, height = 6, units = "in", compression = "lzw", res = 200)
print(spplot(daly_subcatch, names(top_20_rarest), 
             col = NA, 
             layout = c(5,4),
             as.table = TRUE,
             main = list(label = "Area of occupancy (top 20 rarest spp)", cex = 1.3),
             col.regions = color.palette(20),
             sp.layout = list(daly_subcatchment_layer_outline_list)))
dev.off()
