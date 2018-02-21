
devtools::load_all()

my_resources <- c(
  file.path("R", "create_exp_design.R"),
  file.path("R", "pre_processing_functions.R"),
  file.path("R", "count_persistence.R"),
  file.path("R", "calculate_OF.R"),
  file.path("R", "optimize_actions.R"),
  file.path("R", "plot_trackers.R"),
  file.path("R", "plot_effort_map"),
  file.path("R", "one_run.R"),
  file.path("R", "utility_functions.r"))

my_pkgs <- c("ggplot2", "grid", "gridExtra", "dplyr", "rgeos", "RColorBrewer")

root <- "context"

context::context_log_start()
ctx <- context::context_save(path = root,
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters -----------------------------------------------------------


parameters <- list(
  Exp = 1,
  Replicates = 10,
  no_ITER = 1000000,
  Temp_zero = 1,
  cooling_factor = 0.99999,
  fixed_targets = FALSE,
  occurrence_limits = c(500, 10000),
  target_limits = c(1, 0.1),
  TargetLevel = c(seq(50, 1000, 50), seq(1250, 10000, 250)),
  spf = 10,
  print_every_iter = 5)


# load context ----------------------------------------------------------------


context::context_load(ctx)


# create combinations of factor -----------------------------------------------


exp_des <- create_exp_des(parameters)


# run -------------------------------------------------------------------------


solution <- wrapper(parms = parameters,
                    exp_des = exp_des,
                    site_threat_array = site_threat_array,
                    planning_unit = planning_unit,
                    cons_feat_array = cons_feat_array,
                    site_species_array = site_species_array,
                    species_responses = species_responses,
                    parallel = TRUE)


# plot ------------------------------------------------------------------------


# temperature, cost and species penalty
lapply(solution, plot_trackers)

# map of priority effort
lapply(solution, plot_effort_map, daly_prj, rivers_prj, outline_prj)


# save solution ---------------------------------------------------------------


devtools::use_data(solution, solution, overwrite = TRUE)

