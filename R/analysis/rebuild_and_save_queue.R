
# ===================================================================
#
#         How to rebuild a queue object from Rich's R package
#
# ===================================================================


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

parameters <- list(
  Exp = 19,
  Replicates = 10,
  Estimate = 1, #with 1=PP_bestGuess; 2=norm_Lb; 3=norm_Ub
  CPValues = 0,
  TargetLevel = c(seq(50, 1000, 50), seq(1250, 10000, 250)))

context::context_log_start()
ctx <- context::context_save(path = root,
                             sources = my_resources,
                             packages = my_pkgs)

didehpc::didehpc_config_global(workdir = workdir)
obj <- didehpc::queue_didehpc(ctx)

all_bundles <- obj$task_bundle_info()

last_bundle <- all_bundles[nrow(all_bundles), "name"]

run_bundle <- obj$task_bundle_get(last_bundle)

run_results <- run_bundle$results()

output_file_name <- sprintf("queue_obj_exp_%s%s", parameters$Exp, sep = ".rds")
  
write_out_rds(run_results, 
              file.path("output", paste("output_exp", parameters$Exp, sep = "_")), 
              output_file_name)
