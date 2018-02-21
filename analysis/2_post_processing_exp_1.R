
devtools::load_all()


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

cost_scale_factor <- 10000


# load data -------------------------------------------------------------------


results_exp <- readRDS(
  file.path("output", paste0("exp_", parameters$Exp), "solution.rds"))

exp_des <- read.csv(
  file.path("output", paste0("exp_", parameters$Exp), "experimental_design.csv"),
  header = TRUE)


# create table of diagnostics -------------------------------------------------


# get diagnostics for each run
run_diagnostics_ls <- lapply(results_exp, get_run_diagnostics)

# traslate
run_diagnostics <- do.call("rbind", run_diagnostics_ls)

# add columns for additional diagnostics to summary table
run_diagnostics <- cbind(run_diagnostics,
                         best = rep(0, nrow(run_diagnostics)),
                         prop_selected = rep(0, nrow(run_diagnostics)))

# get indices of best runs
indices_best_runs <- moving_index_search(run_diagnostics, parameters$Replicates)

# flag best runs in summary table
run_diagnostics[indices_best_runs, "best"] <- 1

# get the index of selected planning units
selected_planning_unit_indices <- lapply(results_exp, get_selected_pu_index)

# calculate the proportion of the catchment which is selected
prop_daly_selected <- vapply(selected_planning_unit_indices,
                             calc_daly_prop_selected,
                             1,
                             treated_area)

# add to summary table
run_diagnostics[, "prop_selected"] <- prop_daly_selected

# convert costs to millions
run_diagnostics[, "cost"] <- (run_diagnostics[, "cost"] * cost_scale_factor) / 1e6

# append summary table to exp design
run_diagnostics_2 <- cbind(exp_des, run_diagnostics)

best_run_id <- which(run_diagnostics_2[, "best"] == 1)

# get only the best solutions
best_run <- run_diagnostics_2[best_run_id, ]


# save summary table ----------------------------------------------------------


write_out_csv(best_run,
              file.path("output", paste0("exp_", parameters$Exp)),
              "summary_table.csv")


# create table of cost breakdown ----------------------------------------------


best_solution <- results_exp[[best_run_id]]$site_action_array

site_threat_array_cat <- get.threat.intensity.category(parameters,
                                                       site_threat_array)

action_costs <- get.action.costs(site_threat_array_cat, planning_unit)

# action intensity scaling factors
scaling_factors <- get.action.costs(site_threat_array_cat)

paper_table <- get_cost_breakdown(site_action_array = best_solution,
                                  treated_area_file = treated_area,
                                  action_cost_list = action_costs,
                                  scaling_factors = scaling_factors)

# scale back costs to original scale
cost_diagnostics <- grep("cost", names(paper_table))
paper_table[, cost_diagnostics] <- (paper_table[, cost_diagnostics] * cost_scale_factor) / 1e6

# convert ha to km2
area_diagnostics <- grep("area|extent", names(paper_table))
paper_table[, area_diagnostics] <- paper_table[, area_diagnostics] / 100


# save table for paper --------------------------------------------------------


write_out_txt(paper_table,
              file.path("output", paste0("exp_", parameters$Exp)),
              "table_for_paper.txt")
