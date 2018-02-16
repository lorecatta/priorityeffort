
library(rgdal)
library(ggplot2)
library(reshape2)
library(dplyr)
library(RColorBrewer)
library(rgeos)

source(file.path("R", "post_processing", "plot_effort_map.R"))
source(file.path("R", "post_processing", "attach_target_col.r"))


# ---------------------------------------- load data 


results_exp <- readRDS(file.path("output", "output_exp_16", "queue_obj_exp_16.rds"))

output_summary_best <- read.csv(file.path("output", "output_exp_16", "run_summary_exp_16.csv"))

daly_subcatch <- readOGR(dsn = file.path("data", "shapefiles"), layer = "Catch_500")

daly_rivers <- readOGR(dsn = file.path("data", "shapefiles"), layer = "River_500")


# ---------------------------------------- define parameters 


run_ids <- c(12, 56, 113)

myColors <- brewer.pal(5, "YlOrBr")[c(2,3,5)] #avoid using the ligthest colour 

out_path <- file.path("figures", "figures_exp_16")

out_file_name <- "effort_map_multi_target.png"

geograhic_CRS <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84

projected_CRS <- CRS('+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')
  
  
# ---------------------------------------- get the catchment outline


daly_subcatch_outline <- gUnaryUnion(daly_subcatch, id = daly_subcatch@data$OID_)


# ---------------------------------------- assign a coordinate system


proj4string(daly_subcatch) <- geograhic_CRS
proj4string(daly_rivers) <- geograhic_CRS
proj4string(daly_subcatch_outline) <- geograhic_CRS


# ---------------------------------------- project


daly_subcatch_proj <- spTransform(daly_subcatch, projected_CRS)
daly_rivers_proj <- spTransform(daly_rivers, projected_CRS)
daly_subcatch_outline_proj <- spTransform(daly_subcatch_outline, projected_CRS) 


# ---------------------------------------- fortify shapefiles 


daly_subcatch_fort <- fortify(daly_subcatch_proj, region = "GridID") %>% 
  mutate(id = as.numeric(id))

daly_rivers_fort <- fortify(daly_rivers_proj)

daly_outline_fort <- fortify(daly_subcatch_outline_proj)


# ---------------------------------------- reshape the data 


all_site_action_arrays <- lapply(results_exp, "[[", 7)

run_dts <- all_site_action_arrays[run_ids]

run_dts_long <- lapply(run_dts, melt)

run_dts_long_t <- lapply(
  seq_along(run_ids), 
  attach_target_col,
  run_dts = run_dts_long, 
  exp_tab = output_summary_best)

three_target_data <- do.call("rbind", run_dts_long_t)

dat_df <- as.data.frame(three_target_data)

colnames(dat_df)[1:2] <-  c("GridID", "Action")


# ---------------------------------------- join the data to the shapefile


dat_df_xy <- left_join(daly_subcatch_fort, dat_df, by = c("id" = "GridID"))

dat_df_xy$target <- as.factor(dat_df_xy$target)

dat_df_xy$value <- factor(dat_df_xy$value, levels = c(1, 2, 3), labels = c("Low", "Medium", "High"))

levels(dat_df_xy$Action) <- c("Shooting of water buffalos", 
                              "Shooting of feral pigs", 
                              "Riparian fencing", 
                              "Chemical spraying of Paragrass")


# ---------------------------------------- plot map of selected effort


dir.create(out_path, FALSE, TRUE)

png(file.path(out_path, out_file_name),
    width = 16,
    height = 10,
    units = "in",
    pointsize = 12,
    res = 300)

p <- ggplot() +
  geom_polygon(data = daly_outline_fort, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               color = NA,
               fill = "white") +
  geom_polygon(data = dat_df_xy,
               aes(x = long, 
                   y = lat,
                   fill = value, 
                   group = group)) +
  geom_path(data = daly_outline_fort,
            aes(x = long,
                y = lat,
                group = group),
            color = "black",
            size = 0.3) +
  geom_path(data = daly_rivers_fort,
            aes(x = long,
                y = lat,
                group = group),
            color = "grey",
            size = 0.2) +
  facet_grid(target~Action) +
  scale_fill_manual(values = myColors,
                    na.translate = FALSE,
                    guide = guide_legend(title = "Management effort")) +
  scale_x_continuous(name = "Longitude") +
  scale_y_continuous(name = "Latitude")

print(p)

dev.off()
