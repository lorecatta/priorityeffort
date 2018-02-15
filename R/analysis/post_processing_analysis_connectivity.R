rm(list = ls())

# load packages
library(context)
library(queuer)
library(didewin)
library(rgdal)
library(sp) 
library(rgeos)
library(dplyr)
library(grid)
library(ggplot2)
library(RColorBrewer)
library(lattice)
library(latticeExtra)

# load R functions
source(file.path("R", "pre_processing", "parameters_exp_17.R"))
source(file.path("R", "pre_processing", "create_exp_design.R"))
source(file.path("R", "pre_processing", "pre_processing_functions.R"))

source(file.path("R", "processing", "count_persistence.R"))

source(file.path("R", "post_processing", "summary_of_runs.R"))
source(file.path("R", "post_processing", "plot_effort_map.R"))
source(file.path("R", "post_processing", "calculate_species_representation_error.R")) 

# load data
cons_feat_array_df <- read.csv (file.path("data", "input_files", "Conservation Feature.csv"), header = TRUE, sep = ",")
site_species_array_df <- read.csv (file.path("data", "input_files","Planning Unit vs. Conservation Feature.csv"), header=TRUE, sep = ",")
site_threat_array_df <- read.csv (file.path("data", "input_files", "Planning Unit vs. Threat.csv"), header = TRUE, sep = ",")
planning_unit_df <- read.csv (file.path("data", "input_files", "Planning Unit.csv"), header = TRUE, sep = ",")
species_responses_df <- read.csv (file.path("data", "input_files", "Species Responses.csv"), header = TRUE, sep = ",")
daly_planning_unit_area <- read.table (file.path("data","planning_unit_area_ha.txt"), header = TRUE, sep = ",")
daly_subcatch <- readOGR(dsn = file.path("data", "shapefiles"), layer = "Catch_500")
daly_rivers <- readOGR(dsn = file.path("data", "shapefiles"), layer = "River_500")

# rebuild original queue 
root <- "context"
ctx <- context::context_save(packages=c("ggplot2", "grid", "gridExtra"),
                             sources=c(file.path("R", "pre_processing", "parameters_exp_17.R"),
                                       file.path("R", "pre_processing", "create_exp_design.R"),
                                       file.path("R", "pre_processing", "pre_processing_functions.R"),
                                       file.path("R", "processing", "count_persistence.R"),
                                       file.path("R", "processing", "calculate_OF_v3.R"),
                                       file.path("R", "processing", "optimize_actions_v3.R"),
                                       file.path("R", "processing", "plot_optimize_v3.R"),
                                       file.path("R", "processing", "one_run_v3.R")), 
                             root=root)
obj <- didewin::queue_didewin(ctx)

exp_task_bundle_id <- "reconcilable_toucan"

exp <- obj$task_bundle_get(exp_task_bundle_id)

# get job results 
results_exp <- exp$results()

# convert df to matrices 
cons_feat_array <- as.matrix (cons_feat_array_df)
site_species_array <- as.matrix (site_species_array_df)
site_threat_array <- as.matrix (site_threat_array_df)
planning_unit <- as.matrix (planning_unit_df)
species_responses <- as.matrix (species_responses_df)

# create experimental design
exp.des <- create.exp.des (parameters, 0)

# create summary table of all runs 
output_summary <- summary.runs(parameters, exp.des, results_exp, daly_planning_unit_area)

# scale back costs to original scale 
output_summary[, "cost"] <- output_summary[, "cost"] / 100

# get only the best solutions 
output_summary_best <- as.data.frame(output_summary[which(output_summary[, "best"] == 1), ])

#### Plot map of selected effort from different runs ----------------------------------------------------------

# check key characteristics of each layer (data, coordinates, projection, etc) 
#summary (daly_subcatch)
#summary (daly_rivers)

for (i in 1:length(output_summary_best$ID_run))
{
  my_id <- output_summary_best$ID_run[i]
  
  plot.effort.maps (run_id = my_id, 
                    allresults = results_exp, 
                    summary_results = output_summary_best, 
                    daly_subcatchment_layer = daly_subcatch, 
                    daly_river_layer = daly_rivers)
}

#### Plot connectivity map ----------------------------------------------------------

# get indices of solutions you want to plot
low_conn_sol <- 4
med_conn_sol <- 47
high_conn_sol <- 128 
allresults <- results_exp
daly_subcatchment_layer <- daly_subcatch
daly_river_layer <- daly_rivers
  
#exp_id <- unique(summary_results[,"ID_exp"])

# get site action array from specific run 
site_action_array <- data.frame(allresults[[low_conn_sol]][[7]])

# create GridID column
site_action_array <- cbind(site_action_array, GridID = seq(1, nrow(site_action_array), 1))

daly_subcatchment_layer_2 <- daly_subcatchment_layer
  
# join site action array to the shapefile 
daly_subcatchment_layer_2@data <- left_join(daly_subcatchment_layer_2@data, site_action_array, by = "GridID")

# convert levels of effort to factors 
daly_subcatchment_layer_2@data$Buffalo <- factor(daly_subcatchment_layer_2@data$Buffalo, levels=c(1,2,3), labels=c("Low","Medium","High"))
daly_subcatchment_layer_2@data$Pig <- factor(daly_subcatchment_layer_2@data$Pig, levels=c(1,2,3), labels=c("Low","Medium","High"))
daly_subcatchment_layer_2@data$Grazing <- factor(daly_subcatchment_layer_2@data$Grazing, levels=c(1,2,3), labels=c("Low","Medium","High"))
daly_subcatchment_layer_2@data$Weed <- factor(daly_subcatchment_layer_2@data$Weed, levels=c(1,2,3), labels=c("Low","Medium","High"))

# dissolve subcatchment layer to get region outline
daly_subcatchment_layer_outline <- gUnaryUnion(daly_subcatchment_layer_2, id = daly_subcatchment_layer_2@data$OID_)

# define geographic coordinate system / long,lat 
geograhic_CRS <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84

proj4string(daly_subcatchment_layer_2) <- geograhic_CRS
proj4string(daly_river_layer) <- geograhic_CRS
proj4string(daly_subcatchment_layer_outline) <- geograhic_CRS

# define projected coordinate system / eastings and northings (GDA94/Australian Albers, EPSG:3577) 
# look up http://www.spatialreference.org/
projected_CRS <- CRS("+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

daly_subcatchment_layer_2_proj <- spTransform(daly_subcatchment_layer_2, projected_CRS)
daly_river_layer_proj <- spTransform(daly_river_layer, projected_CRS)
daly_subcatchment_layer_outline_proj <- spTransform(daly_subcatchment_layer_outline, projected_CRS) 

# get coordinates box for helping placing arrow and scale bar later  
bb <- bbox(daly_subcatchment_layer_2_proj) 

# ------------------------ Prepare layer for medium degree of connectivity -----------------------------------------

# get site action array from specific run 
site_action_array <- data.frame(allresults[[med_conn_sol]][[7]])

# create GridID column
site_action_array <- cbind(site_action_array, GridID = seq(1, nrow(site_action_array), 1))

daly_subcatchment_layer_3 <- daly_subcatchment_layer

# join site action array to the shapefile 
daly_subcatchment_layer_3@data <- left_join(daly_subcatchment_layer_3@data, site_action_array, by = "GridID")

# convert levels of effort to factors 
daly_subcatchment_layer_3@data$Buffalo <- factor(daly_subcatchment_layer_3@data$Buffalo, levels=c(1,2,3), labels=c("Low","Medium","High"))
daly_subcatchment_layer_3@data$Pig <- factor(daly_subcatchment_layer_3@data$Pig, levels=c(1,2,3), labels=c("Low","Medium","High"))
daly_subcatchment_layer_3@data$Grazing <- factor(daly_subcatchment_layer_3@data$Grazing, levels=c(1,2,3), labels=c("Low","Medium","High"))
daly_subcatchment_layer_3@data$Weed <- factor(daly_subcatchment_layer_3@data$Weed, levels=c(1,2,3), labels=c("Low","Medium","High"))

proj4string(daly_subcatchment_layer_3) <- geograhic_CRS
daly_subcatchment_layer_3_proj <- spTransform(daly_subcatchment_layer_3, projected_CRS)

# ------------------------ Prepare layer for high degree of connectivity -----------------------------------------

# get site action array from specific run 
site_action_array <- data.frame(allresults[[high_conn_sol]][[7]])

# create GridID column
site_action_array <- cbind(site_action_array, GridID = seq(1, nrow(site_action_array), 1))

daly_subcatchment_layer_4 <- daly_subcatchment_layer

# join site action array to the shapefile 
daly_subcatchment_layer_4@data <- left_join(daly_subcatchment_layer_4@data, site_action_array, by = "GridID")

# convert levels of effort to factors 
daly_subcatchment_layer_4@data$Buffalo <- factor(daly_subcatchment_layer_4@data$Buffalo, levels=c(1,2,3), labels=c("Low","Medium","High"))
daly_subcatchment_layer_4@data$Pig <- factor(daly_subcatchment_layer_4@data$Pig, levels=c(1,2,3), labels=c("Low","Medium","High"))
daly_subcatchment_layer_4@data$Grazing <- factor(daly_subcatchment_layer_4@data$Grazing, levels=c(1,2,3), labels=c("Low","Medium","High"))
daly_subcatchment_layer_4@data$Weed <- factor(daly_subcatchment_layer_4@data$Weed, levels=c(1,2,3), labels=c("Low","Medium","High"))

proj4string(daly_subcatchment_layer_4) <- geograhic_CRS
daly_subcatchment_layer_4_proj <- spTransform(daly_subcatchment_layer_4, projected_CRS)

# --------------------------------------- Prepare plotting objects --------------------------------------

# create list objects for shapefiles to overaly 
daly_river_layer_list <- list("sp.lines", 
                              daly_river_layer_proj, 
                              col = "grey")

daly_subcatchment_layer_outline_list <- list("sp.polygons", 
                                             daly_subcatchment_layer_outline_proj, 
                                             col = "black", 
                                             first = FALSE)

north_arrow <- list("SpatialPolygonsRescale", 
                    layout.north.arrow(), 
                    offset = c(bb[1,1] + 60000, bb[2,1] + 42000), 
                    scale = 60000, 
                    which = 4)

scale_bar <- list("SpatialPolygonsRescale", 
                  layout.scale.bar(), 
                  offset = c(bb[1,1] + 20000, bb[2,1] + 27000), 
                  scale = 100000, 
                  fill = c("transparent", "black"), 
                  which = 4)

# labels for scale bar
text1 <- list("sp.text", c(bb[1,1] + 23000, bb[2,1] + 12000), "0", cex = 1.1, which = 4) 
text2 <- list("sp.text", c(bb[1,1] + 155000, bb[2,1] + 12000), "100 km", cex = 1.1, which = 4) 

# Useful for checking parameters of trellis graphics
#names(trellis.par.get())
#trellis.par.get("strip.border")

###
# NOTE: How to costumize the key (legend) in a spplot with polygons
###
# Disable the default colorkey (colorkey=FALSE) and define a custom key using the "key"
# argument. It expects a list whose components define the elements of the
# legend. You should read the help page of lattice::xyplot for details about "key".

myColors <- brewer.pal(5, "YlOrBr")[c(2,3,5)] #avoid using the ligthest colour 

myKey <- list(text = list(lab = levels(daly_subcatchment_layer_2_proj@data$Buffalo)),
              rectangles = list(col = myColors, border = FALSE),
              space = "right",
              columns = 1,
              title = "Management effort",
              cex.title = 1,
              padding.text = 3)

low_conn_plot <- spplot(daly_subcatchment_layer_2_proj, c("Buffalo", "Pig", "Grazing", "Weed"), col = NA,
             names.attr = c("Shooting of water buffalos", "Shooting of feral pigs", "Riparian fencing", "Chemical spraying of Paragrass"),
             layout = c(4,1),
             as.table = TRUE,
             strip = FALSE,
             colorkey = FALSE,
             key = myKey,
             col.regions = myColors,
             par.settings = list(axis.line = list(col = "transparent"),
                                 strip.background = list(col = "transparent"),
                                 strip.border = list(lty = 0)),
             par.strip.text = list(col = "black", cex = 0.9),
             sp.layout = list(daly_river_layer_list,
                              daly_subcatchment_layer_outline_list,
                              scale_bar,
                              text1,
                              text2,
                              north_arrow))

med_conn_plot <- spplot(daly_subcatchment_layer_3_proj, c("Buffalo", "Pig", "Grazing", "Weed"), col = NA,
                        layout = c(4,1),
                        as.table = TRUE,
                        strip = FALSE,
                        colorkey = FALSE,
                        key = myKey,
                        col.regions = myColors,
                        par.settings = list(axis.line = list(col = "transparent")),
                        sp.layout = list(daly_river_layer_list,
                                         daly_subcatchment_layer_outline_list))

high_conn_plot <- spplot(daly_subcatchment_layer_4_proj, c("Buffalo", "Pig", "Grazing", "Weed"), col = NA,
                        layout = c(4,1),
                        as.table = TRUE,
                        strip = FALSE,
                        colorkey = FALSE,
                        key = myKey,
                        col.regions = myColors,
                        par.settings = list(axis.line = list(col = "transparent")),
                        sp.layout = list(daly_river_layer_list,
                                         daly_subcatchment_layer_outline_list))

plot.file.name <- sprintf("Effort_maps_by_CMS_exp_%s%s", parameters$Exp, ".tiff")

tiff(filename = file.path("figures", paste("figures_exp", parameters$Exp, sep = "_"), 
                          plot.file.name),
     width = 30, height = 20, units = "cm", pointsize = 12,
     compression = "lzw",
     bg = "white", res = 300)

c(low_conn_plot, med_conn_plot, high_conn_plot, layout = c(4,3), merge.legends = FALSE)
dev.off()

#update(comb_plot, strip = function(..., which.panel, var.name) strip.default(..., which.panel = 1, var.name = c("Shooting of water buffalos", "Shooting of feral pigs", "Riparian fencing", "Chemical spraying of Paragrass")))
