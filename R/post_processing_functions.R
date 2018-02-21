get_run_diagnostics <- function(solution) {

  #cat("run =", j, "\n") #debugging

  diagnostics <- c("cost",
                   "species_benefit",
                   "species_penalty",
                   "OF",
                   "planning_units")

  # create output
  output <- setNames(rep(0, length(diagnostics)), diagnostics)

  output[] <- c(solution$cost,
                solution$species_benefit,
                solution$species_penalty,
                solution$OF_value,
                solution$no_pu)

  output

}

moving_index_search <- function(my_data, runs){

  d <- seq_along(my_data[, "OF"])
  d1 <- split(my_data[, "OF"], ceiling(d / runs))
  mins <- sapply(d1, which.min)
  indices <- sapply(unique(ceiling(d / runs)), function(i, mins) {mins[i] + (runs * (i - 1))}, mins)
  as.numeric(indices)

}

get_selected_pu_index <- function(solution){

  a <- apply(solution$site_action_array, 1, function(y) min(sum(y), 1))

  which(a == 1)

}

calc_daly_prop_selected <- function(sel_pu_ind, area_file){

  # NOTE: The treated are for buffalo control IS the planning unit area
  total_area <- sum(area_file[, "buffalo"])

  selected_area <- sum(area_file[sel_pu_ind, "buffalo"])

  selected_area / total_area

}

get_cost_breakdown <- function(site_action_array,
                               treated_area_file,
                               action_cost_list,
                               scaling_factors) {

  no.sites <- nrow(site_action_array)
  no.threats <- ncol(site_action_array)

  no_years <- 20

  actions <- c("Aerial shooting of buffalos",
               "Aerial shooting of pigs",
               "Riparian fencing",
               "Chemical spraying")

  levels_of_effort <- c("Low", "Medium", "High")

  diagnostics <- c("annual_cost",
                   "20y_cost",
                   "treated_area")

  output <- expand.grid(actions, levels_of_effort)

  output <- cbind(output, matrix(rep(0, length(diagnostics) * nrow(output)),
                                 ncol = length(diagnostics)))

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

  output[output$Action == "Aerial shooting of buffalos", "annual_cost"] <- c(buffalo_cost_low,
                                                                             buffalo_cost_medium,
                                                                             buffalo_cost_high)

  output[output$Action == "Aerial shooting of pigs", "annual_cost"] <- c(pig_cost_low,
                                                                         pig_cost_medium,
                                                                         pig_cost_high)

  output[output$Action == "Riparian fencing", "annual_cost"] <- c(grazing_cost_low,
                                                                  grazing_cost_medium,
                                                                  grazing_cost_high)

  output[output$Action == "Chemical spraying", "annual_cost"] <- c(weed_cost_low,
                                                                   weed_cost_medium,
                                                                   weed_cost_high)

  output[output$Action == "Aerial shooting of buffalos", "20y_cost"] <- output[output$Action == "Aerial shooting of buffalos", "annual_cost"] * no_years
  output[output$Action == "Aerial shooting of pigs", "20y_cost"] <- output[output$Action == "Aerial shooting of pigs", "annual_cost"] * no_years
  output[output$Action == "Riparian fencing", "20y_cost"] <- output[output$Action == "Riparian fencing", "annual_cost"] * no_years
  output[output$Action == "Chemical spraying", "20y_cost"] <- output[output$Action == "Chemical spraying", "annual_cost"] * no_years


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

  pu_selected_low_buffalo <- length(which(site_action_array[, "buffalo"] == 1))
  pu_selected_medium_buffalo <- length(which(site_action_array[, "buffalo"] == 2))
  pu_selected_high_buffalo <- length(which(site_action_array[, "buffalo"] == 3))

  pu_selected_low_pig <- length(which(site_action_array[, "pig"] == 1))
  pu_selected_medium_pig <- length(which(site_action_array[, "pig"] == 2))
  pu_selected_high_pig <- length(which(site_action_array[, "pig"] == 3))

  pu_selected_low_grazing <- length(which(site_action_array[, "grazing"] == 1))
  pu_selected_medium_grazing <- length(which(site_action_array[, "grazing"] == 2))
  pu_selected_high_grazing <- length(which(site_action_array[, "grazing"] == 3))

  pu_selected_low_weed <- length(which(site_action_array[, "weed"] == 1))
  pu_selected_medium_weed <- length(which(site_action_array[, "weed"] == 2))
  pu_selected_high_weed <- length(which(site_action_array[, "weed"] == 3))

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
        treated_area_value <- treated_area_file[i, "buffalo"]
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
        treated_area_value <- treated_area_file[i, "pig"]
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
        treated_area_value <- treated_area_file[i, "grazing"]
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
        treated_area_value <- treated_area_file[i, "weed"]
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
    }
  }

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

  output[order(output$Action),]

}
