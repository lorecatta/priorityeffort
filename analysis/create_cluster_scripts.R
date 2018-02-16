###Create character string of all functions to call

rm(list=ls())

# load functions 
source(file.path("R", "pre_processing", "parameters_v3.R"))
source(file.path("R", "pre_processing", "create_exp_design.R"))

exp.des <- create.exp.des (parameters, 0)

Repeat <- '# load packages 
library(ggplot2)
library(grid)
library(gridExtra)

# load R functions
source(file.path("R", "pre_processing", "parameters_v3.R"))
source(file.path("R", "pre_processing", "create_exp_design.R"))
source(file.path("R", "pre_processing", "pre_processing_functions.R"))
source(file.path("R", "processing", "count_persistence.R"))
source(file.path("R", "processing", "calculate_OF_v3.R"))
source(file.path("R", "processing", "optimize_actions_v3.R"))
source(file.path("R", "processing", "plot_optimize_v3.R"))
source(file.path("R", "processing", "one_run_v3.R"))

# load data
boundary.file_df <- read.csv (file.path("data", "input_files","Boundary.csv"), header=TRUE, sep = ",")
cons_feat_array_df <- read.csv (file.path("data","input_files","Conservation Feature.csv"), header=TRUE, sep = ",")
site_species_array_df <- read.csv (file.path("data", "input_files","Planning Unit vs. Conservation Feature.csv"), header=TRUE, sep = ",")
site_threat_array_df <- read.csv (file.path("data", "input_files","Planning Unit vs. Threat.csv"), header=TRUE, sep = ",")
planning_unit_df <- read.csv (file.path("data", "input_files","Planning Unit.csv"), header=TRUE, sep = ",")
species_responses_df <- read.csv (file.path("data", "input_files","Species Responses.csv"), header=TRUE, sep = ",")

# convert to matrices 
boundary.file <- as.matrix (boundary.file_df)
cons_feat_array <- as.matrix (cons_feat_array_df)
site_species_array <- as.matrix (site_species_array_df)
site_threat_array <- as.matrix (site_threat_array_df)
planning_unit <- as.matrix (planning_unit_df)
species_responses <- as.matrix (species_responses_df)

# pre-processing
exp.des <- create.exp.des (parameters, 0)
site_threat_array_cat <- get.threat.intensity.category (parameters, site_threat_array)
all_site_action_int_combs <- get.site.action.intensities.combs (parameters, site_threat_array_cat)
all_upstream_connections <- get.upstream_connections (boundary.file, site_species_array)
all_downstream_connections <- get.downstream_connections (boundary.file, site_species_array)
action_costs <- get.action.costs (site_threat_array_cat, planning_unit)

# create folders for output spreadsheets
dir.create(file.path("output", paste("output_exp", parameters$Exp, sep="_"), "annealing_summaries"), FALSE, TRUE)
dir.create(file.path("output", paste("output_exp", parameters$Exp, sep="_"), "best_solutions"), FALSE, TRUE)
dir.create(file.path("output", paste("output_exp", parameters$Exp, sep="_"), "R_workspaces"), FALSE, TRUE)

# create folder for output figures 
dir.create(file.path("figures", paste("figures_exp", parameters$Exp, sep="_")), FALSE, TRUE)
'

#create individual scripts
index <- 1

for (i in 1:nrow(exp.des))
{
  Apply <- sprintf("\nRun%d <- apply (exp.des[%d,,drop=F],1, 
       one.run, 
       parameters, 
       cons_feat_array, 
       all_site_action_int_combs, 
       action_costs, 
       site_threat_array_cat, 
       site_species_array, 
       species_responses, 
       all_upstream_connections, 
       boundary.file, 
       all_downstream_connections)", i, i)

  script_name <- sprintf("script_%d.R", i)
  
  write(paste(Repeat,Apply), file.path("hpc", "scripts", script_name))

  index <- index + 1
}
