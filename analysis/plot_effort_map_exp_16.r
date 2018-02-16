
library(rgdal)
library(ggplot2)
library(reshape2)
library(dplyr)
library(RColorBrewer)
library(rgeos)

source(file.path("R", "post_processing", "plot_effort_map.R"))
source(file.path("R", "post_processing", "attach_target_col.r"))


# ---------------------------------------- define parameters


exp_ID <- 18

ID_I_want_to_plot <- 8


# ---------------------------------------- load data 


results_exp <- readRDS(file.path("output", paste0("output_exp_", exp_ID), paste0("queue_obj_exp_", exp_ID, ".rds")))

output_summary_best <- read.csv(file.path("output", 
                                          paste0("output_exp_", exp_ID), 
                                          paste0("run_summary_exp_", exp_ID, ".csv")))

daly_subcatch <- readOGR(dsn = file.path("data", "shapefiles"), layer = "Catch_500")

daly_rivers <- readOGR(dsn = file.path("data", "shapefiles"), layer = "River_500")


# ---------------------------------------- plot


plot.effort.maps(run_id = ID_I_want_to_plot, 
                 allresults = results_exp, 
                 summary_results = output_summary_best, 
                 daly_subcatchment_layer = daly_subcatch, 
                 daly_river_layer = daly_rivers)
