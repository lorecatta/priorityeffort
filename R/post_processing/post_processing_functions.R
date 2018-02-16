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
