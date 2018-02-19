###
# function for creating a matrix of the category (0,1,2,3) of threat intensity in each site
###
get.threat.intensity.category <- function(parms, site_threat_array) {
  #Set the category of threat intensity in a site.
  #Possible categories of threat intensity:
  #0: no threat
  #1: low intensity
  #2: medium intensity
  #3: high intensity

  #define variables within the function
  no.sites <- nrow(site_threat_array)
  no.threats <- ncol(site_threat_array)

  out_mat <- matrix(0, nrow=no.sites, ncol=no.threats)
  colnames(out_mat) <- 1:dim(out_mat)[2]
  colnames(out_mat) <- colnames(site_threat_array)

  for (i in 1:no.sites)
  {

    for (j in 1:no.threats)
    {

      if(site_threat_array[i,j] <= 0.334 & site_threat_array[i,j]!=0)
      {

        out_mat[i,j] <- 1

      }

      if(site_threat_array[i,j] > 0.334 & site_threat_array[i,j] <= 0.667)
      {

        out_mat[i,j] <- 2

      }

      if(site_threat_array[i,j] > 0.667 & site_threat_array[i,j] <= 1)
      {

        out_mat[i,j] <- 3

      }

    }

  }

  out_mat
}

###
# function for creating a matrix of all available levels of effort, for each action, at each site
###
get.site.action.intensities.combs <- function(parms, site_threat_array_cat, no.levels = 3) {
  #Set the category of action intensity in a site.
  #Possible categories of action intensity (including No selection):
  #0: No selection
  #1: Maintain current threat intensity - Maintain species value
  #2: Reduces threat intensity of 1 category - Restore species value
  #3: Reduces threat intensity of 2 categories - Restore species value
  #4: Reduces threat intensity of 3 categories - Restore species value  - I AM REMOVING THIS ONE NOW 25-05-2016

  #define variables within the function
  no.sites <- nrow(site_threat_array_cat)
  no.threats <- ncol(site_threat_array_cat)

  no_threats_low_int <- length(which(site_threat_array_cat[,1:no.threats]==1))
  no_threats_medium_int <- length(which(site_threat_array_cat[,1:no.threats]==2))
  no_threats_high_int <- length(which(site_threat_array_cat[,1:no.threats]==3))

  no_actions_threats_low_int <- no_threats_low_int * (no.levels-2)
  no_actions_threats_medium_int <- no_threats_medium_int * (no.levels-1)
  no_actions_threats_high_int <- no_threats_high_int * no.levels

  total_no_combs <- no_actions_threats_low_int + no_actions_threats_medium_int + no_actions_threats_high_int

  all_combs <- matrix(0, nrow=(total_no_combs), ncol=3)
  colnames(all_combs) <- 1:dim(all_combs)[2]
  colnames(all_combs) <- c("site", "action", "intensity")

  index <- 1

  for (i in seq_len(no.sites))
  {

    site <- i
    #cat("site =", site, "\n") #debugging

    for (j in 1:no.threats)
    {

      #cat("threat =", j, "\n") #debugging

      threat_intensity_category <- site_threat_array_cat[site,j]
      #cat("Threat intensity category = ", threat_intensity_category, "\n") #debugging

      if(threat_intensity_category > 0)
      {

        if(threat_intensity_category == 1)
        {

          PUs <- rep(site, (no.levels-2))
          action <- rep(j, (no.levels-2))
          intensities <- seq_len(no.levels-2)
          all_combs[index:(index+length(intensities)-1),"site"] <- PUs
          all_combs[index:(index+length(intensities)-1),"action"] <- action
          all_combs[index:(index+length(intensities)-1),"intensity"] <- intensities

        }

        if(threat_intensity_category == 2)
        {

          PUs <- rep(site, (no.levels-1))
          action <- rep(j, (no.levels-1))
          intensities <- seq_len(no.levels-1)
          all_combs[index:(index+length(intensities)-1),"site"] <- PUs
          all_combs[index:(index+length(intensities)-1),"action"] <- action
          all_combs[index:(index+length(intensities)-1),"intensity"] <- intensities

        }

        if(threat_intensity_category == 3)
        {

          PUs <- rep(site, no.levels)
          action <- rep(j, no.levels)
          intensities <- seq_len(no.levels)
          all_combs[index:(index+length(intensities)-1),"site"] <- PUs
          all_combs[index:(index+length(intensities)-1),"action"] <- action
          all_combs[index:(index+length(intensities)-1),"intensity"] <- intensities

        }

        index <- index + length(intensities)
        #cat("index =", index, "\n") #debugging
      }

    }

  }

  all_combs
}

###
# function for creating a list with the costs of all levels of effort available for each action at each site
###
get.action.costs <- function(site_threat_array_cat, planning_unit = NULL, no.levels = 3) {
  #define variables within the function
  no.sites <- nrow(site_threat_array_cat)
  no.threats <- ncol(site_threat_array_cat)

  #create list for storing action costs
  cost_list <- lapply(1:no.sites, matrix, data=0, nrow=no.levels+1, ncol=no.threats)

  full_cost <- 1

  for (i in seq_len(no.sites))
  {

    site <- i
    #cat("site = ", site, "\n") #debugging

    for (j in 1:no.threats)
    {

      #cat("threat = ", j, "\n") #debugging

      if(!is.null(planning_unit))
      {
        full_cost <- planning_unit[site, j]
        #cat("full_cost = ", full_cost, "\n") #debugging
      }

      threat_intensity <- site_threat_array_cat[site,j]

      if(threat_intensity == 1)
      {

        linear_scaling_factors <- (1/threat_intensity) * seq_len(no.levels-2)
        cost_list[[site]][2,j] <- linear_scaling_factors * full_cost

      }

      if(threat_intensity == 2)
      {

        linear_scaling_factors <- (1/threat_intensity) * seq_len(no.levels-1)
        cost_list[[site]][2:3,j] <- linear_scaling_factors * full_cost

      }

      if(threat_intensity == 3)
      {

        linear_scaling_factors <- (1/threat_intensity) * seq_len(no.levels)
        cost_list[[site]][2:4,j] <- linear_scaling_factors * full_cost

      }

    }

  }

  cost_list
}

###
# function for creating a list of the actions required to abate all threats to each species
###
get.required_actions <- function(site_threat_array_cat, responses_to_actions, cons_feat_array) {
  no.sites <- nrow(site_threat_array_cat)
  no.species <- nrow(cons_feat_array)
  no.threats <- ncol(site_threat_array_cat)

  output <- lapply(1:no.sites, matrix, data=0, nrow=no.threats, ncol=no.species)

  for (i in 1:no.sites)
  {

    for (j in 1:no.threats)
    {
      #cat("threat = ", j, "\n") #debugging

      for (z in 1:no.species)
      {

        #cat("species = ", z, "\n") #debugging

        threat_intensity_cat <- site_threat_array_cat[i,j]

        if(threat_intensity_cat > 0)
        {

          index <- z + (no.species*(j-1))
          #cat("index = ", index, "\n") #debugging

          # get the prob of persistence achieved with the highest level of effort
          output[[i]][j,z] <- max(responses_to_actions[[index]][,threat_intensity_cat+1])

        }

      }

    }

  }

  aggregate_output_1 <- lapply(output, function(x){apply(x, 2, sum)})
  aggregate_output_2 <- do.call(rbind, aggregate_output_1)

  aggregate_output_2
}

###
# function for creating a list of the values of probability of persistence of each species for each level of effort of each action
###
get.responses.to.actions <- function(species_responses, cons_feat_array, estimate, no.levels = 3) {
  #define variables within the function
  no.species <- nrow(cons_feat_array)
  no.threats <- length(unique(species_responses[,"Threat"]))

  no_of_threat_intensity_categories <- 4

  #create list for storing species benefits
  combs <- no.species * no.threats
  out_list <- lapply(1:combs, matrix, data = 0, nrow = no.levels+1, ncol = no_of_threat_intensity_categories)

  for (j in 1:no.threats)
  {

    for (z in 1:no.species)
    {

      index <- z + (no.species*(j-1))
      #cat("index =", index, "\n") #debugging

      faunalG <- cons_feat_array[z,"FaunalGroup"]
      #cat("faunal group =", faunalG, "\n") #debugging

      ecologicalG <- cons_feat_array[z,"EcologicalGroup"]
      #cat("ecological group =", ecologicalG, "\n") #debugging

      if(estimate == 1)
      {

        response_values <- species_responses[which(species_responses [,"FaunalGroup"] == faunalG
                                                   & species_responses [,"EcologicalGroup"] == ecologicalG
                                                   & species_responses [,"Threat"] == j), "PP_BestGuess"]

      }

      if(estimate == 2)
      {

        response_values <- species_responses[which(species_responses [,"FaunalGroup"] == faunalG
                                                   & species_responses [,"EcologicalGroup"] == ecologicalG
                                                   & species_responses [,"Threat"] == j), "norm_lower"]

      }

      if(estimate == 3)
      {

        response_values <- species_responses[which(species_responses [,"FaunalGroup"] == faunalG
                                                   & species_responses [,"EcologicalGroup"] == ecologicalG
                                                   & species_responses [,"Threat"] == j), "norm_upper"]

      }

      out_list [[index]][2,2] <- response_values[1]
      out_list [[index]][2:3,3] <- response_values[c(2,1)]
      out_list [[index]][2:4,4] <- response_values[order(response_values, decreasing = FALSE)]

    }

  }

  out_list
}

###
# function for setting targets
###
set_fixed_targets <- function(cons_feat_array, site_species_array, target.level) {

  Target_values <- rep(0, nrow(cons_feat_array))

  # get total area of occupancy for each species
  tot_occ_area_per_species <- as.numeric(apply(site_species_array,2,sum))

  # get target values for species with area of occupancy smaller than the target
  occupancy_smaller_than_target <- which(tot_occ_area_per_species <= target.level)
  Target_values[occupancy_smaller_than_target] <- tot_occ_area_per_species[occupancy_smaller_than_target]

  # get target values for species with area of occupancy larger than the target
  occupancy_larger_than_target <- which(tot_occ_area_per_species > target.level)
  Target_values[occupancy_larger_than_target] <- rep(target.level, length(occupancy_larger_than_target))

  # edit input file
  cons_feat_array[, "target"] <- Target_values

  cons_feat_array
}

set_scaled_targets <- function(cons_feat_array, occurrence_limits, target_limits) {

  # species targets are linearly scaled to species' area of occupancy

  interp_targ <- function(i, a, b) approx(a, b, xout = i)$y

  cons_feat_array <- cbind(cons_feat_array, target_perc = 0)

  below_lim_1 <- which(cons_feat_array[, "area_of_occ"] < occurrence_limits[1])
  above_lim_2 <- which(cons_feat_array[, "area_of_occ"] > occurrence_limits[2])

  cons_feat_array[below_lim_1, "target_perc"] <- target_limits[1] * 100
  cons_feat_array[above_lim_2, "target_perc"] <- target_limits[2] * 100

  cons_feat_array[below_lim_1, "target"] <- round(cons_feat_array[below_lim_1, "area_of_occ"] * target_limits[1],
                                                  digits = 2)
  cons_feat_array[above_lim_2, "target"] <- round(cons_feat_array[above_lim_2, "area_of_occ"] * target_limits[2],
                                                  digits = 2)

  between_lims_ids <- which(cons_feat_array[, "target"] == 0)
  between_lims <- cons_feat_array[between_lims_ids, ]

  targets <- vapply(between_lims[, "area_of_occ"],
                    interp_targ,
                    numeric(1),
                    a = occurrence_limits,
                    b = target_limits)

  cons_feat_array[between_lims_ids, "target_perc"] <- targets * 100

  cons_feat_array[between_lims_ids, "target"] <- round(cons_feat_array[between_lims_ids, "area_of_occ"] * targets,
                                                       digits = 2)

  cbind(ID = seq_len(nrow(cons_feat_array)), cons_feat_array)

}
