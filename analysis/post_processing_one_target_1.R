# load packages
library(rgdal)
library(dplyr)
library(ggplot2)


# ---------------------------------------- define parameters


parameters <- list(
  Exp = 19,
  Replicates = 10,
  Estimate = 1, #with 1=PP_bestGuess; 2=norm_Lb; 3=norm_Ub
  CPValues = 0,
  TargetLevel = c(seq(50, 1000, 50), seq(1250, 10000, 250)))

summary_table_name <- sprintf("run_summary_%s%s", paste("exp", parameters$Exp, sep="_"), ".csv")

output_fl_nm <- "table_2_for_paper.txt"


# ---------------------------------------- load other function


source(file.path("R", "pre_processing", "create_exp_design.R"))
source(file.path("R", "pre_processing", "pre_processing_functions.R"))
source(file.path("R", "post_processing", "get_cost_breakdown_by_level_&_action.R"))
source(file.path("R", "post_processing", "post_processing_functions.R"))
source(file.path("R", "utility_functions.r"))

# load data
site_species_array_df <- read.csv(file.path("data", "input_files","Planning Unit vs. Conservation Feature.csv"), header=TRUE, sep = ",")
site_threat_array_df <- read.csv(file.path("data", "input_files", "Planning Unit vs. Threat.csv"), header = TRUE, sep = ",")
planning_unit_df <- read.csv(file.path("data", "input_files", "Planning Unit.csv"), header = TRUE, sep = ",")
treated_area_df <- read.csv(file.path("data","Treated_area.csv"), header=TRUE, sep = ",")
daly_planning_unit_area <- read.table(file.path("data","planning_unit_area_ha.txt"), header = TRUE, sep = ",")

daly_subcatch <- readOGR(dsn = file.path("data", "shapefiles"), layer = "Catch_500")
daly_rivers <- readOGR(dsn = file.path("data", "shapefiles"), layer = "River_500")

results_exp <- readRDS(
  file.path("output", 
            paste0("output_exp_", parameters$Exp), 
            paste0("queue_obj_exp_", parameters$Exp, ".rds")))


# ---------------------------------------- start


# convert df to matrices 
site_threat_array <- as.matrix (site_threat_array_df)
planning_unit <- as.matrix (planning_unit_df)
treated_area <- as.matrix(treated_area_df)

# create experimental design
exp.des <- create.exp.des (parameters, 0)


# ---------------------------------------- create summary table of simulation diagnostics


# get diagnostics for each run 
run_diagnostics <- sapply(
  seq_along(results_exp),
  get_run_diagnostics,
  all_run_results = results_exp)

# traslate 
run_diagnostics_t <- t(run_diagnostics)

# add columns for additional diagnostics to summary table 
run_diagnostics_t <- cbind(run_diagnostics_t, 
                           best = rep(0, nrow(run_diagnostics_t)),
                           prop_selected = rep(0, nrow(run_diagnostics_t)))

# get indices of best runs 
indices_best_runs <- moving_index_search(run_diagnostics_t, parameters$Replicates)

# flag best runs in summary table
run_diagnostics_t[indices_best_runs, "best"] <- 1

# get the index of selected planning units 
selected_planning_unit_indices <- lapply(results_exp, get_selected_pu_index)

# get the total area of the Daly catchment
total_area <- sum(daly_planning_unit_area[, "Shape_Area"])

# add proportion of total area which has been selected 
run_diagnostics_t[, "prop_selected"] <- sapply(
  seq_along(results_exp), 
  calc_daly_prop_selected, 
  selected_planning_unit_indices, 
  daly_planning_unit_area,
  total_area)

# scale back costs to original scale 
run_diagnostics_t[, "cost"] <- run_diagnostics_t[, "cost"] / 100

# append summary table to exp design
run_diagnostics_t_2 <- cbind(exp.des, run_diagnostics_t)

# get only the best solutions 
output_summary_best <- as.data.frame(run_diagnostics_t_2[which(run_diagnostics_t_2[, "best"] == 1), ])


# ---------------------------------------- save output


# write out summary table 
write_out_csv(output_summary_best, 
              file.path("output", paste("output_exp", parameters$Exp, sep="_")), 
              summary_table_name)


# ---------------------------------------- create table of cost breakdown 


ID_I_want_to_plot <- 2

best_solution <- results_exp[[ID_I_want_to_plot]][[7]]

site_threat_array_cat <- get.threat.intensity.category(parameters, site_threat_array)

action_costs <- get.action.costs(site_threat_array_cat, planning_unit)

action_intensity_scaling_factors <- get.action.costs(site_threat_array_cat)

table_2 <- get_cost_breakdown(site_action_array = best_solution,
                              treated_area_file = treated_area, 
                              action_cost_list = action_costs, 
                              scaling_factors = action_intensity_scaling_factors)

# scale back costs to original scale 
cost_based_diagnostics <- grep("cost", names(table_2)) 
table_2[, cost_based_diagnostics] <- table_2[, cost_based_diagnostics] / 100

# convert ha to km2
area_based_diagnostics <- grep("area|extent", names(table_2)) 
table_2[, area_based_diagnostics] <- table_2[, area_based_diagnostics] / 100


# ---------------------------------------- write out outputs


# create folders for species summaries 
dir.create(file.path("output", 
                     paste("output_exp", parameters$Exp, sep="_")), 
           FALSE, TRUE)

# write out cost breakdown table 
write.table(table_2, 
            file.path("output", 
                      paste("output_exp", parameters$Exp, sep = "_"), 
                      output_fl_nm), 
            row.names = FALSE, 
            sep = ",")
