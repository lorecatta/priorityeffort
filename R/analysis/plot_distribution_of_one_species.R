
library(ggplot2)
library(rgdal)
library(rgeos)
library(dplyr)
library(RColorBrewer)



# ---------------------------------------- load data


site_species_array_df <- read.csv(
  file.path("data", 
            "input_files",
            "Planning Unit vs. Conservation Feature.csv"), 
  header = TRUE)

cons_feat_array_df <- read.csv(
  file.path("data",
            "input_files",
            "Conservation Feature.csv"), 
  header = TRUE,
  stringsAsFactors = FALSE)

daly_subcatch <- readOGR(dsn = file.path("data", "shapefiles"), layer = "Catch_500")


# ----------------------------------------


# to remove 0 values from plotting 
site_species_array_df[site_species_array_df == 0] <- NA

# dissolve subcatchment layer to get region outline
daly_subcatchment_layer_outline <- gUnaryUnion(daly_subcatch, id = daly_subcatch@data$OID_)

names(site_species_array_df) <- cons_feat_array_df$sp_names

daly_subcatch_fort <- fortify(daly_subcatch, region = "GridID") %>% 
  mutate(id = as.numeric(id))

daly_outline_fort <- fortify(daly_subcatchment_layer_outline)

site_species_array_df$GridID <- seq_len(nrow(site_species_array_df))

dat_df_xy <- left_join(daly_subcatch_fort, site_species_array_df, by = c("id" = "GridID"))

YlOrRd <- brewer.pal(9, "YlOrRd")[c(2,5,7,9)]
color.palette <- colorRampPalette(YlOrRd) # this is a function!

out_nm <- "one_species_distribution.png"

png(file.path("figures", out_nm), 
    width = 4, 
    height = 3, 
    units = "in", 
    pointsize = 12,
    res = 300)

p <- ggplot() +
  geom_polygon(data = dat_df_xy,
               aes(x = long, 
                   y = lat,
                   fill = cra_ster, 
                   group = group)) +
  geom_path(data = daly_outline_fort,
            aes(x = long,
                y = lat,
                group = group),
            color = "black",
            size = 0.5) +
  scale_fill_gradientn(colors = color.palette(100),
                       na.value = "white",
                       guide = guide_colourbar(title = "",
                                               barheight = 4, 
                                               barwidth = 1.5,
                                               label = FALSE)) +
  scale_x_continuous(limits = c(130,134)) +
  theme_void() +
  theme(legend.text = element_text(size = 20),
        legend.title = element_text(size = 10),
        legend.position = c(0.15, 0.2),
        legend.key.size = unit(0.5, "in"),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))
           
print(p)

dev.off()
