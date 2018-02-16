###
# The following functions are used for running the post processing analysis 
###

get_run_diagnostics <- function(j, all_run_results) {
  
  #cat("run =", j, "\n") #debugging
  
  diagnostics <- c("cost", 
                   "species_benefit", 
                   "species_penalty", 
                   "conn_penalty", 
                   "OF", 
                   "planning_units")
  
  # create output
  output <- setNames(rep(0, length(diagnostics)), diagnostics)
  
  output["cost"] <- all_run_results[[j]][[2]]
  output["species_benefit"] <- all_run_results[[j]][[3]]
  output["species_penalty"] <- all_run_results[[j]][[4]]
  output["conn_penalty"] <- all_run_results[[j]][[5]]
  output["OF"] <- all_run_results[[j]][[6]]
  output["planning_units"] <- all_run_results[[j]][[9]]
  
  output

}

moving_index_search <- function(my_data, runs){
  
  d <- seq_along(my_data[, "OF"])
  d1 <- split(my_data[, "OF"], ceiling(d / runs))
  mins <- sapply(d1, which.min)
  indices <- sapply(unique(ceiling(d / runs)), function(i, mins) {mins[i] + (runs * (i - 1))}, mins)
  as.numeric(indices)
  
}

get_selected_pu_index <- function(x){
  
  a <- apply(x[[7]], 1, function(y) min(sum(y), 1))
  b <- which(a == 1)
  b
  
}

calc_daly_prop_selected <- function(i, sel_pu_ind, area_file, total_area){
  
  a <- sel_pu_ind[[i]]
  selected_area <- sum(area_file[a, "Shape_Area"])
  out <- selected_area / total_area
  
}


# # used in loop below 
# OF_value_vec [j - amount_sub] <- all_run_results[[j]][[6]]
# status_list [[j - amount_sub]] <- all_run_results[[j]][[7]]    
# 
# if(j %% no.runs == 0) {
#   
#   #get index of the replicate, of the first set of replicates of unique combinations of factors, with the lowest objective function
#   min_OF_Values <- which(OF_value_vec == min(OF_value_vec))
#   replicate_index <- min_OF_Values[1]
#   #cat("replicate_index = ", replicate_index, "\n") #debugging
#   
#   #get index of the run corresponding to the replicate
#   #index_shift <- stop_points[which(stop_points < j)[length(which(stop_points < j))]]
#   #run_index <- replicate_index + index_shift
#   #cat("run_index = ", run_index, "\n") #debugging
#   #run_label_suffix <- paste(paste("Run", run_index, sep=""), ".txt", sep="")
#   
#   #flag the runs as the best among the replicates 
#   output["best"] <- 1        
#   
#   ###Output site action array
#   best_solution <- status_list[[replicate_index]]
#   #best_sol_name <- paste("best_solution_", run_label_suffix, sep="")
#   #best_sol_path_to_file <- file.path("output", folder_of_run_summary, best_sol_name)
#   #write.table(best_solution, best_sol_path_to_file, sep = ",", row.names = T, col.names=NA)
#   
#   ###Output selection frequency
#   sel_frequency <- Reduce("+", status_list)
#   #sel_freq_name <- paste("selection_frequency_", run_label_suffix, sep="")
#   #sel_freq_path_to_file <- file.path("output", folder_of_run_summary, sel_freq_name)
#   #write.table(sel_frequency, sel_freq_path_to_file, sep = ",", row.names = TRUE, col.names=NA)
#   
#   #reboot friends
#   OF_value_vec <- c()
#   status_list <- vector("list", no.runs)
#   amount_sub <- amount_sub + no.runs
# }
# 
# 
# experiment_folder_name <- sprintf("output_%s", paste("exp", exp_id, sep="_"))
# 
# ## get info for species summaries 
# no_species <- length(all_run_results[[j]][[10]])
# 
# summary_species <- data.frame(species_id = seq_len(no_species))
# summary_species$target <- all_run_results[[j]][[10]]
# summary_species$benefit_achieved <- as.numeric(apply(all_run_results[[j]][[8]], 2, sum))
# 
# no_species_targets_missed <- nrow(summary_species[round(summary_species$benefit_achieved, 1) < round(summary_species$target, 1), ])
# output["no_missed_targets"] <- no_species_targets_missed
# 
# species_summary_name <- sprintf("species_summary_%s_%s%s", paste("exp", exp_id, sep="_"), paste("run", j, sep="_"), ".csv")
# write.table(summary_species, file.path("output", experiment_folder_name, "species_summaries", species_summary_name), sep = ",", row.names = FALSE)
