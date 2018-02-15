
options(didehpc.cluster = "fi--didemrchnb")

my_resources <- c(
  file.path("R", "pre_processing", "create_exp_design.R"),
  file.path("R", "pre_processing", "pre_processing_functions.R"),
  file.path("R", "processing", "count_persistence.R"),
  file.path("R", "processing", "calculate_OF_v3.R"),
  file.path("R", "processing", "optimize_actions_v3.R"),
  file.path("R", "processing", "plot_optimize_v3.R"),
  file.path("R", "processing", "one_run_v3.R"),
  file.path("R", "utility_functions.r"))
  
my_pkgs <- c("ggplot2", "grid", "gridExtra")

workdir <- "Q:/version_3"
root <- file.path(workdir, "contexts")

context::context_log_start()
ctx <- context::context_save(path = root,
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- define parameters 


CLUSTER <- TRUE

parameters <- list(
  Exp = 19,
  Replicates = 10,
  Estimate = 1, #with 1=PP_bestGuess; 2=norm_Lb; 3=norm_Ub
  CPValues = 0,
  TargetLevel = c(seq(50, 1000, 50), seq(1250, 10000, 250)))
  
occurrence_limits <- c(500, 10000)

target_limits <- c(1, 0.1)

base_info <- c("area_of_occ", "target", "spf", "FaunalGroup", "EcologicalGroup")

input_pt <- file.path("data", "input_files")


# ---------------------------------------- are you using the cluster? 


if (CLUSTER) {
  
  didehpc::didehpc_config_global(workdir = workdir)
  obj <- didehpc::queue_didehpc(ctx)
  
} else {
  
  context::context_load(ctx)
  
}


# ---------------------------------------- load data


boundary.file_df <- read.csv(
  file.path(input_pt, "Boundary.csv"), 
  header = TRUE)

cons_feat_array_df <- read.csv(
  file.path(
    input_pt,"Conservation Feature.csv"), 
  header = TRUE)

site_species_array_df <- read.csv(
  file.path(input_pt, "Planning Unit vs. Conservation Feature.csv"), 
  header = TRUE)

site_threat_array_df <- read.csv(
  file.path(input_pt, "Planning Unit vs. Threat.csv"), 
  header = TRUE)

planning_unit_df <- read.csv(
  file.path(input_pt, "Planning Unit.csv"), 
  header = TRUE)

species_responses_df <- read.csv(
  file.path(input_pt, "Species Responses.csv"), 
  header = TRUE)


# ---------------------------------------- linearly scaled species targets to species' area of occupancy 


#cons_feat_array_df <- set_scaled_targets(cons_feat_array_df, occurrence_limits, target_limits)


# ---------------------------------------- pre processing


boundary.file <- setNames(as.matrix(boundary.file_df), colnames(boundary.file_df))
cons_feat_array <- setNames(as.matrix(cons_feat_array_df[, base_info]), colnames(cons_feat_array_df[, base_info]))
site_species_array <- setNames(as.matrix(site_species_array_df), colnames(site_species_array_df))
site_threat_array <- setNames(as.matrix(site_threat_array_df), colnames(site_threat_array_df))
planning_unit <- setNames(as.matrix(planning_unit_df), colnames(planning_unit_df))
species_responses <- setNames(as.matrix(species_responses_df), colnames(species_responses_df))

exp.des <- create.exp.des(parameters, 0)
exp_des_ls <- df_to_list(exp.des, use_names = TRUE)
  
site_threat_array_cat <- get.threat.intensity.category(parameters, site_threat_array)
all_site_action_int_combs <- get.site.action.intensities.combs(parameters, site_threat_array_cat)
action_costs <- get.action.costs(site_threat_array_cat, planning_unit)

if(file.exists(file.path("output", "upstream_connections.rds"))) {
  
  all_upstream_connections <- readRDS(file.path("output", "upstream_connections.rds"))

} else {
  
  all_upstream_connections <- get.upstream_connections(boundary.file, site_species_array)
  
  saveRDS(all_upstream_connections, file.path("output", "upstream_connections.rds"))
  
}
  
if(file.exists(file.path("output", "downstream_connections.rds"))) {
  
  all_downstream_connections <- readRDS(file.path("output", "downstream_connections.rds"))
  
} else {
  
  all_downstream_connections <- get.downstream_connections(boundary.file, site_species_array)
  
  saveRDS(all_downstream_connections, file.path("output", "downstream_connections.rds"))
  
}


# ---------------------------------------- run one job


# t <- obj$enqueue(
#   one.run(exp_des_ls[[1]],
#           parameters, 
#           cons_feat_array, 
#           all_site_action_int_combs, 
#           action_costs, 
#           site_threat_array_cat, 
#           site_species_array, 
#           species_responses, 
#           all_upstream_connections, 
#           boundary.file, 
#           all_downstream_connections))

  
# ---------------------------------------- run multiple jobs


if (CLUSTER) {
  
  run <- queuer::qlapply(
    exp_des_ls, 
    one.run,
    obj,
    cons_feat_array, 
    all_site_action_int_combs, 
    action_costs, 
    site_threat_array_cat, 
    site_species_array, 
    species_responses, 
    all_upstream_connections, 
    boundary.file, 
    all_downstream_connections)
  
} else {
  
  run <- lapply(
    exp_des_ls[1],
    one.run, 
    cons_feat_array, 
    all_site_action_int_combs, 
    action_costs, 
    site_threat_array_cat, 
    site_species_array, 
    species_responses, 
    all_upstream_connections, 
    boundary.file, 
    all_downstream_connections)
  
}
