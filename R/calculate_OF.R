#' Calculate the value of the objective function.
#'
#' @param parameters a list of parameters.
#' @param cons_feat_array a matrix of information (columns) for each conservation
#'   feature (rows). Information include: area of occupancy, target level, species
#'   penalty factor (spf), faunal group id and ecological group id.
#' @param all_site_action_int_combs a matrix of all the combinations (rows) of site,
#'   action and level of effort (columns) available for selection in the study area.
#' @param site_action_array a matrix of the selected effort for each action (columns)
#'   in each site (rows).
#' @param action_costs a list of matrices with the cost of selecting each level of effort.
#'   Each matrix stores the cost of implementing each level of effort (rows) for each
#'   action (columns) in each site (list slot). The list has lenght equals to the
#'   number of site.
#' @param site_threat_array_cat a matrix of the category of magnitute for each threat
#'   in each site.
#' @param responses_to_actions a list of matrices with the benefit of selecting each
#'   level of effort. Each matrix stores the benefit of implementing each level of effort
#'   (rows) for different threat category at the site (columns) for each site-action
#'   combination (list slot). The list has lenght equals to the combination of site
#'   and actions.
#' @param site_species_array a matrix of the area of occupancy of each feature (columns)
#'   in each site (rows).
#' @param required_actions a list of the total number of threats (rows) affecting
#'   each species (columns) at each site (list slot).
#'
#' @export
components_OF <- function(parameters,
                          cons_feat_array,
                          all_site_action_int_combs,
                          site_action_array,
                          action_costs,
                          site_threat_array_cat,
                          responses_to_actions,
                          site_species_array,
                          required_actions){

  #define variables within the function
  no.sites <- nrow(site_action_array)
  no.species <- nrow(cons_feat_array)
  no.actions <- ncol(site_action_array)
  Target <- cons_feat_array[,"target"]
  S.P.F <- cons_feat_array[,"spf"]

  Start_prop <- parameters$start_prop

  #create list for recording probabilty of persistence
  SpeciesCount_list <- lapply(1:no.sites, matrix, data = 0, nrow = no.actions, ncol = no.species)

  #create matrix for counting costs
  CostCount_mat <- matrix(0, nrow = no.sites, ncol = no.actions)

  #get unique combinations of sites and actions
  all_site_action_combs <- aggregate(all_site_action_int_combs[,c(1,2)],
                                     list(all_site_action_int_combs[,"action"],
                                          all_site_action_int_combs[,"site"]),
                                     mean)

  #initial allocation of intensities to actions within sites
  for (i in 1:nrow(all_site_action_combs)) {

    rnd_number <- runif(n = 1, min = 0, max = 1)

    if (rnd_number < Start_prop) {

      site <- all_site_action_combs[i, "site"]
      action <- all_site_action_combs[i, "action"]

      available_action_intensities <- all_site_action_int_combs[which(all_site_action_int_combs[, "site"] == site & all_site_action_int_combs[, "action"] == action), "intensity"]

      resample <- function(x,n) x[sample.int(length(x),n)]

      action_intensity <- resample(available_action_intensities, 1)

      site_action_array [site, action] <- action_intensity

    }

  }

  for (j in seq_len(no.sites)) {

    site <- j
    #cat("site =", site, "\n") #debugging

    for (action in 1:no.actions) {

      #cat("action =", action, "\n") #debugging

      run_count <- count.persistence(site,
                                     action,
                                     site_action_array,
                                     action_costs,
                                     site_threat_array_cat,
                                     responses_to_actions)

      CostCount_mat [site,action] <- run_count[[1]]
      SpeciesCount_list [[site]][action,] <- run_count[[2]]

    }

  }

  SpeciesCount_list_sum <- lapply(SpeciesCount_list, function(x){apply(x, 2, sum)})

  SpeciesCount_mat <- do.call(rbind, SpeciesCount_list_sum)

  #calculate species benefit at each site
  SpeciesBenefit_mat <- pmin(SpeciesCount_mat / required_actions, 1) * site_species_array

  #calculate total species benefit
  SpeciesBenefit <- apply(SpeciesBenefit_mat, 2, sum)

  #calculate penalty count
  PenaltyCount_vec <- pmax.int(Target - SpeciesBenefit, 0)
  PenaltyCount <- sum(PenaltyCount_vec)

  #calculate total cost for all sites
  CostCount <- sum(CostCount_mat)

  #calculate value of the objective function
  OF_value <- CostCount + sum(S.P.F * PenaltyCount_vec)

  #create empty list of outputs
  list_of_outputs <- vector("list", 6)

  #fill the list of outputs
  list_of_outputs [[1]] <- c(CostCount = CostCount, PenaltyCount = PenaltyCount, OF_value = OF_value)
  list_of_outputs [[2]] <- SpeciesBenefit
  list_of_outputs [[3]] <- site_action_array
  list_of_outputs [[4]] <- SpeciesCount_list
  list_of_outputs [[5]] <- SpeciesBenefit_mat
  list_of_outputs [[6]] <- CostCount_mat

  #return output
  list_of_outputs

}
