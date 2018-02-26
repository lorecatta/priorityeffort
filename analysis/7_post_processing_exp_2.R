
devtools::load_all()

library(ggplot2)
library(reshape2)


# define parameters ---------------------------------------


parameters <- list(
  Exp = 2,
  Replicates = 10,
  no_ITER = 1000000,
  Temp_zero = 1,
  cooling_factor = 0.99999,
  fixed_targets = TRUE,
  occurrence_limits = c(500, 10000),
  target_limits = c(1, 0.1),
  TargetLevel = seq(500, 10000, 500),
  spf = 10,
  print_every_iter = 10)

response_types <- c("best_guess", "lower_bound", "upper_bound")


# load data -------------------------------------------------------------------


results_exp <- readRDS(
  file.path("output", paste0("exp_", parameters$Exp), "solution.rds"))

output_summary_best <- read.csv(
  file.path("output", paste0("exp_", parameters$Exp), "summary_table.csv"),
  header = TRUE)


# create some arrays ----------------------------------------------------------


response_list <- lapply(seq_along(response_types),
                        wrapper_to_get_responses,
                        species_responses,
                        cons_feat_array)

responses_to_actions_BG <- response_list[[1]]

site_threat_array_cat <- get.threat.intensity.category(parameters,
                                                       site_threat_array)

required.actions <- get.required_actions (site_threat_array_cat,
                                          responses_to_actions_BG,
                                          cons_feat_array)

action_costs <- get.action.costs (site_threat_array_cat, planning_unit)


# -----------------------------------------------------------------------------
#
# analysis of uncertainty
#
# -----------------------------------------------------------------------------


factor_combs_df <- expand.grid(target_level = parameters$TargetLevel,
                               response_type = seq_along(response_types))

factor_combs_df$ID_exp <- unique(output_summary_best$ID_exp)
factor_combs_df$ID_run <- rep(output_summary_best$ID_run, length(response_types))

factor_combs <- df_to_list(factor_combs_df, use_names = TRUE)

all_exp_des_runs <- lapply(factor_combs,
                           calculate_representation_error,
                           b = response_list,
                           c = response_types,
                           parms = parameters,
                           cons_features = cons_feat_array,
                           site_threat_array_cat.mat = site_threat_array_cat,
                           all_run_results = results_exp,
                           action_cost_list = action_costs,
                           responses_to_actions_EXP = responses_to_actions_BG,
                           required_actions = required.actions,
                           site_species_array.mat = site_species_array,
                           output_by_species = TRUE)

all_errors <- lapply(all_exp_des_runs, calc_uncertainty_diagnostics)

all_errors <- do.call("rbind", all_errors)


# plot percentage change ------------------------------------------------------


data_to_plot <- cbind(factor_combs_df, all_errors)

data_to_plot$response_type <- factor(data_to_plot$response_type,
                                     labels = response_types)

plot_representation_error(aa = data_to_plot,
                          parms = parameters,
                          analysis = analysis_sub_directory,
                          by_var = "response_type")


# plot species above/below/at target ------------------------------------------


data_to_plot_long <- melt(data_to_plot,
                          id.vars = c("target_level", "response_type"),
                          measure.vars = c("prop_below", "prop_above"),
                          variable.name = "species_prop")

levels(data_to_plot_long$species_prop) <- c("Below target", "Above target")


plot_species_proportions(aa = data_to_plot_long,
                         parms = parameters,
                         analysis = analysis_sub_directory,
                         by_var = "response_type")


# plot percentage change from target ------------------------------------------

# mean (negative and positive) percentage change from target (+_SE)

data_to_plot_long_2 <- reshape(data_to_plot,
                               varying = 14:17,
                               sep = "_",
                               direction = "long")

data_to_plot_long_2$time <- factor(data_to_plot_long_2$time,
                                   levels = c("neg", "pos"),
                                   labels = c("Below target", "Above target"))

data_to_plot_long_2$response_type <- factor(data_to_plot_long_2$response_type,
                                            levels = c("best_guess", "lower_bound", "upper_bound"),
                                            labels = c("Best guess", "Lower bound", "Upper bound"))


plot_change_from_target(data_to_plot_long_2, parameters)

