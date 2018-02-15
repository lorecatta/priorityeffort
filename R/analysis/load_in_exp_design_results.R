###
# The function loads all the R environments of different simulations and put them together in a list 
###
load.in <- function(experimental_design)
{
  exp_id <- unique(experimental_design[, "ID_exp"])
  
  no_of_runs <- nrow(experimental_design)
  
  allresults <- vector("list", 2)
  
  # get name of experiment result folder
  experiment_folder_name <- sprintf("output_%s", paste("exp", exp_id, sep="_"))
  
  #load runs
  for (i in 1:no_of_runs)
  {
    #readRDS(sprintf("%s%s%d%s",  file.path("output", experiment_folder_name, "R_workspaces"), "/Run_", i, ".rds"))
    
    #obj <- sprintf("Run%d", i)  
    
    #allresults[[i]] <- get(obj)[[1]]
    allresults[[i]] <- readRDS(sprintf("%s%s%d%s",  file.path("output", experiment_folder_name, "R_workspaces"), "/Run_", i, ".rds"))
  }
  
  allresults
}
