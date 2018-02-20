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

get_cost_breakdown <- function(site_action_array,
                               treated_area_file,
                               action_cost_list,
                               scaling_factors) {

  no.sites <- nrow(site_action_array)
  no.threats <- ncol(site_action_array)

  actions <- c("Aerial shooting of buffalos",
               "Aerial shooting of pigs",
               "Riparian fencing",
               "Chemical spraying")

  levels_of_effort <- c("Low", "Medium", "High")

  diagnostics <- c("cost_per_year", "20y_cost", "threat_extent", "treated_area", "selected_PU", "treated_area_per_PU")

  output <- expand.grid(actions, levels_of_effort)

  output <- cbind(output, matrix(rep(0, length(diagnostics) * nrow(output)), ncol = length(diagnostics)))

  names(output) <- c("Action", "Level", diagnostics)

  buffalo_cost_low <- 0
  buffalo_cost_medium <- 0
  buffalo_cost_high <- 0

  pig_cost_low <- 0
  pig_cost_medium <- 0
  pig_cost_high <- 0

  grazing_cost_low <- 0
  grazing_cost_medium <- 0
  grazing_cost_high <- 0

  weed_cost_low <- 0
  weed_cost_medium <- 0
  weed_cost_high <- 0

  for (i in 1:no.sites)
  {

    for (j in 1:no.threats)
    {

      action_intensity <- site_action_array[i, j]

      action_intensity_cost <- action_cost_list[[i]][action_intensity + 1, j]

      if(j == 1)
      {
        if(action_intensity == 1)
        {
          buffalo_cost_low <- buffalo_cost_low + action_intensity_cost
        }
        if(action_intensity == 2)
        {
          buffalo_cost_medium <- buffalo_cost_medium + action_intensity_cost
        }
        if(action_intensity == 3)
        {
          buffalo_cost_high <- buffalo_cost_high + action_intensity_cost
        }
      }
      if(j == 2)
      {
        if(action_intensity == 1)
        {
          pig_cost_low <- pig_cost_low + action_intensity_cost
        }
        if(action_intensity == 2)
        {
          pig_cost_medium <- pig_cost_medium + action_intensity_cost
        }
        if(action_intensity == 3)
        {
          pig_cost_high <- pig_cost_high + action_intensity_cost
        }
      }
      if(j == 3)
      {
        if(action_intensity == 1)
        {
          grazing_cost_low <- grazing_cost_low + action_intensity_cost
        }
        if(action_intensity == 2)
        {
          grazing_cost_medium <- grazing_cost_medium + action_intensity_cost
        }
        if(action_intensity == 3)
        {
          grazing_cost_high <- grazing_cost_high + action_intensity_cost
        }
      }
      if(j == 4)
      {
        if(action_intensity == 1)
        {
          weed_cost_low <- weed_cost_low + action_intensity_cost
        }
        if(action_intensity == 2)
        {
          weed_cost_medium <- weed_cost_medium + action_intensity_cost
        }
        if(action_intensity == 3)
        {
          weed_cost_high <- weed_cost_high + action_intensity_cost
        }
      }
    }
  }

  output[output$Action == "Aerial shooting of buffalos", "cost_per_year"] <- c(buffalo_cost_low, buffalo_cost_medium, buffalo_cost_high)
  output[output$Action == "Aerial shooting of pigs", "cost_per_year"] <- c(pig_cost_low, pig_cost_medium, pig_cost_high)
  output[output$Action == "Riparian fencing", "cost_per_year"] <- c(grazing_cost_low, grazing_cost_medium, grazing_cost_high)
  output[output$Action == "Chemical spraying", "cost_per_year"] <- c(weed_cost_low, weed_cost_medium, weed_cost_high)

  output[output$Action == "Aerial shooting of buffalos", "20y_cost"] <- output[output$Action == "Aerial shooting of buffalos", "cost_per_year"] * 20
  output[output$Action == "Aerial shooting of pigs", "20y_cost"] <- output[output$Action == "Aerial shooting of pigs", "cost_per_year"] * 20
  output[output$Action == "Riparian fencing", "20y_cost"] <- output[output$Action == "Riparian fencing", "cost_per_year"] * 20
  output[output$Action == "Chemical spraying", "20y_cost"] <- output[output$Action == "Chemical spraying", "cost_per_year"] * 20

  # ----------------------------- same but for extent of treatment ---------------------------------

  prop_treated_area_low_buffalo <- 0
  all_treated_area_low_buffalo <- 0
  prop_treated_area_medium_buffalo <- 0
  all_treated_area_medium_buffalo <- 0
  prop_treated_area_high_buffalo <- 0
  all_treated_area_high_buffalo <- 0

  prop_treated_area_low_pig <- 0
  all_treated_area_low_pig <- 0
  prop_treated_area_medium_pig <- 0
  all_treated_area_medium_pig <- 0
  prop_treated_area_high_pig <- 0
  all_treated_area_high_pig <- 0

  prop_treated_area_low_grazing <- 0
  all_treated_area_low_grazing <- 0
  prop_treated_area_medium_grazing <- 0
  all_treated_area_medium_grazing <- 0
  prop_treated_area_high_grazing <- 0
  all_treated_area_high_grazing <- 0

  prop_treated_area_low_weed <- 0
  all_treated_area_low_weed <- 0
  prop_treated_area_medium_weed <- 0
  all_treated_area_medium_weed <- 0
  prop_treated_area_high_weed <- 0
  all_treated_area_high_weed <- 0

  pu_selected_low_buffalo <- length(which(site_action_array[, "Buffalo"] == 1))
  pu_selected_medium_buffalo <- length(which(site_action_array[, "Buffalo"] == 2))
  pu_selected_high_buffalo <- length(which(site_action_array[, "Buffalo"] == 3))

  pu_selected_low_pig <- length(which(site_action_array[, "Pig"] == 1))
  pu_selected_medium_pig <- length(which(site_action_array[, "Pig"] == 2))
  pu_selected_high_pig <- length(which(site_action_array[, "Pig"] == 3))

  pu_selected_low_grazing <- length(which(site_action_array[, "Grazing"] == 1))
  pu_selected_medium_grazing <- length(which(site_action_array[, "Grazing"] == 2))
  pu_selected_high_grazing <- length(which(site_action_array[, "Grazing"] == 3))

  pu_selected_low_weed <- length(which(site_action_array[, "Weed"] == 1))
  pu_selected_medium_weed <- length(which(site_action_array[, "Weed"] == 2))
  pu_selected_high_weed <- length(which(site_action_array[, "Weed"] == 3))

  for (i in 1:no.sites)
  {

    #cat("site = ", i, "\n") #debugging
    for (j in 1:no.threats)
    {

      #cat("action = ", j, "\n") #debugging
      action_intensity <- site_action_array[i, j]

      action_intensity_scaling_factor <- action_intensity_scaling_factors[[i]][action_intensity+1, j]

      if(j == 1)
      {
        treated_area_value <- treated_area_file[i, "Buffalo_treated_area"]
        if(action_intensity == 1)
        {
          prop_treated_area_low_buffalo <- prop_treated_area_low_buffalo + (treated_area_value * action_intensity_scaling_factor)
          all_treated_area_low_buffalo <- all_treated_area_low_buffalo + treated_area_value
        }
        if(action_intensity == 2)
        {
          prop_treated_area_medium_buffalo <- prop_treated_area_medium_buffalo + (treated_area_value * action_intensity_scaling_factor)
          all_treated_area_medium_buffalo <- all_treated_area_medium_buffalo + treated_area_value
        }
        if(action_intensity == 3)
        {
          prop_treated_area_high_buffalo <- prop_treated_area_high_buffalo + (treated_area_value * action_intensity_scaling_factor)
          all_treated_area_high_buffalo <- all_treated_area_high_buffalo + treated_area_value
        }
      }
      if(j == 2)
      {
        treated_area_value <- treated_area_file[i, "Pig_treated_area"]
        if(action_intensity == 1)
        {
          prop_treated_area_low_pig <- prop_treated_area_low_pig + (treated_area_value * action_intensity_scaling_factor)
          all_treated_area_low_pig <- all_treated_area_low_pig + treated_area_value
        }
        if(action_intensity == 2)
        {
          prop_treated_area_medium_pig <- prop_treated_area_medium_pig + (treated_area_value * action_intensity_scaling_factor)
          all_treated_area_medium_pig <- all_treated_area_medium_pig + treated_area_value
        }
        if(action_intensity == 3)
        {
          prop_treated_area_high_pig <- prop_treated_area_high_pig + (treated_area_value * action_intensity_scaling_factor)
          all_treated_area_high_pig <- all_treated_area_high_pig + treated_area_value
        }
      }
      if(j == 3)
      {
        treated_area_value <- treated_area_file[i, "Grazing_treated_area"]
        if(action_intensity == 1)
        {
          prop_treated_area_low_grazing <- prop_treated_area_low_grazing + (treated_area_value * action_intensity_scaling_factor)
          all_treated_area_low_grazing <- all_treated_area_low_grazing + treated_area_value
        }
        if(action_intensity == 2)
        {
          prop_treated_area_medium_grazing <- prop_treated_area_medium_grazing + (treated_area_value * action_intensity_scaling_factor)
          all_treated_area_medium_grazing <- all_treated_area_medium_grazing + treated_area_value
        }
        if(action_intensity == 3)
        {
          prop_treated_area_high_grazing <- prop_treated_area_high_grazing + (treated_area_value * action_intensity_scaling_factor)
          all_treated_area_high_grazing <- all_treated_area_high_grazing + treated_area_value
        }
      }
      if(j == 4)
      {
        treated_area_value <- treated_area_file[i, "Weed_treated_area"]
        if(action_intensity == 1)
        {
          prop_treated_area_low_weed <- prop_treated_area_low_weed + (treated_area_value * action_intensity_scaling_factor)
          all_treated_area_low_weed <- all_treated_area_low_weed + treated_area_value
        }
        if(action_intensity == 2)
        {
          prop_treated_area_medium_weed <- prop_treated_area_medium_weed + (treated_area_value * action_intensity_scaling_factor)
          all_treated_area_medium_weed <- all_treated_area_medium_weed + treated_area_value
        }
        if(action_intensity == 3)
        {
          prop_treated_area_high_weed <- prop_treated_area_high_weed + (treated_area_value * action_intensity_scaling_factor)
          all_treated_area_high_weed <- all_treated_area_high_weed + treated_area_value
        }
      }
      #cat("buffalo_area_high_2 = ", buffalo_area_high_2, "\n") #debugging
    }
  }

  output[output$Action == "Aerial shooting of buffalos", "threat_extent"] <- c(all_treated_area_low_buffalo,
                                                                               all_treated_area_medium_buffalo,
                                                                               all_treated_area_high_buffalo)
  output[output$Action == "Aerial shooting of pigs", "threat_extent"] <- c(all_treated_area_low_pig,
                                                                           all_treated_area_medium_pig,
                                                                           all_treated_area_high_pig)
  output[output$Action == "Riparian fencing", "threat_extent"] <- c(all_treated_area_low_grazing,
                                                                    all_treated_area_medium_grazing,
                                                                    all_treated_area_high_grazing)
  output[output$Action == "Chemical spraying", "threat_extent"] <- c(all_treated_area_low_weed,
                                                                     all_treated_area_medium_weed,
                                                                     all_treated_area_high_weed)

  output[output$Action == "Aerial shooting of buffalos", "treated_area"] <- c(prop_treated_area_low_buffalo,
                                                                              prop_treated_area_medium_buffalo,
                                                                              prop_treated_area_high_buffalo)
  output[output$Action == "Aerial shooting of pigs", "treated_area"] <- c(prop_treated_area_low_pig,
                                                                          prop_treated_area_medium_pig,
                                                                          prop_treated_area_high_pig)
  output[output$Action == "Riparian fencing", "treated_area"] <- c(prop_treated_area_low_grazing,
                                                                   prop_treated_area_medium_grazing,
                                                                   prop_treated_area_high_grazing)
  output[output$Action == "Chemical spraying", "treated_area"] <- c(prop_treated_area_low_weed,
                                                                    prop_treated_area_medium_weed,
                                                                    prop_treated_area_high_weed)

  output[output$Action == "Aerial shooting of buffalos", "selected_PU"] <- c(pu_selected_low_buffalo,
                                                                             pu_selected_medium_buffalo,
                                                                             pu_selected_high_buffalo)
  output[output$Action == "Aerial shooting of pigs", "selected_PU"] <- c(pu_selected_low_pig,
                                                                         pu_selected_medium_pig,
                                                                         pu_selected_high_pig)
  output[output$Action == "Riparian fencing", "selected_PU"] <- c(pu_selected_low_grazing,
                                                                  pu_selected_medium_grazing,
                                                                  pu_selected_high_grazing)
  output[output$Action == "Chemical spraying", "selected_PU"] <- c(pu_selected_low_weed,
                                                                   pu_selected_medium_weed,
                                                                   pu_selected_high_weed)

  output[output$Action == "Aerial shooting of buffalos", "treated_area_per_PU"] <- c(prop_treated_area_low_buffalo / pu_selected_low_buffalo,
                                                                                     prop_treated_area_medium_buffalo / pu_selected_medium_buffalo,
                                                                                     prop_treated_area_high_buffalo / pu_selected_high_buffalo)
  output[output$Action == "Aerial shooting of pigs", "treated_area_per_PU"] <- c(prop_treated_area_low_pig / pu_selected_low_pig,
                                                                                 prop_treated_area_medium_pig / pu_selected_medium_pig,
                                                                                 prop_treated_area_high_pig / pu_selected_high_pig)
  output[output$Action == "Riparian fencing", "treated_area_per_PU"] <- c(prop_treated_area_low_grazing / pu_selected_low_grazing,
                                                                          prop_treated_area_medium_grazing / pu_selected_medium_grazing,
                                                                          prop_treated_area_high_grazing / pu_selected_high_grazing)
  output[output$Action == "Chemical spraying", "treated_area_per_PU"] <- c(prop_treated_area_low_weed / pu_selected_low_weed,
                                                                           prop_treated_area_medium_weed / pu_selected_medium_weed,
                                                                           prop_treated_area_high_weed / pu_selected_high_weed)

  output$treated_area_per_PU[is.na(output$treated_area_per_PU)] <- 0

  output <- output[order(output$Action),]
}
