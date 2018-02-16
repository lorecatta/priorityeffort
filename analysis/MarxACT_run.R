
my_resources <- c(
  file.path("R", "create_exp_design.R"),
  file.path("R", "pre_processing_functions.R"),
  file.path("R", "count_persistence.R"),
  file.path("R", "calculate_OF.R"),
  file.path("R", "optimize_actions.R"),
  file.path("R", "plot_optimize.R"),
  file.path("R", "one_run.R"),
  file.path("R", "utility_functions.r"))

my_pkgs <- c("ggplot2", "grid", "gridExtra")

root <- "context"

context::context_log_start()
ctx <- context::context_save(path = root,
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters -----------------------------------------------------------

parameters <- list(
  Exp = 1,
  Replicates = 10,
  TargetLevel = c(seq(50, 1000, 50), seq(1250, 10000, 250)))


# set up a couple of things ---------------------------------------------------


n_cores <- 3

context::context_load(ctx)
context::parallel_cluster_start(n_cores)

exp.des <- create.exp.des(parameters)
exp_des_ls <- df_to_list(exp.des, use_names = TRUE)


# run -------------------------------------------------------------------------


run <- loop(exp_des_ls[1:3],
            one_run,
            cons_feat_array,
            all_site_action_int_combs,
            action_costs,
            site_threat_array_cat,
            site_species_array,
            species_responses,
            all_upstream_connections,
            boundary.file,
            all_downstream_connections,
            parallel = TRUE)

context::parallel_cluster_stop()
