#' Wrapper for vectorizing \code{get.responses.to.actions()}.
#'
#' @param i the numeric index of the vector
#' @inheritParams components_OF
#'
#' @export
wrapper_to_get_responses <- function(i, species_responses, cons_feat_array) {

  get.responses.to.actions(species_responses, cons_feat_array, estimate = i)

}

#' Calculate a set of diagnostics for a specific assumption regarding reponses
#'   uncertanity.
#'
#' @param a solution object. A list.
#' @param b a list of species-action benefits calculated assuming that the true
#'   species responses corresponds to the experts' best guesses, the lower
#'   bounds and the upper bounds.
#' @param c a character vector of name tags.
#' @param all_run_results a list of solution objects.
#' @param responses_to_actions_EXP a list of matrices with the benefit of selecting each
#'   level of effort. Each matrix stores the benefit of implementing each level of effort
#'   (rows) for different threat category at the site (columns) for each site-action
#'   combination (list slot). The list has lenght equals to the combination of site
#'   and actions. Response values are calculated assuming that the true species
#'   responses corresponds to the experts' best guesses.
#' @param output_by_species a logical indicating wheather a table for each species is saved.
#' @inheritParams components_OF
#'
#' @export
calculate_representation_error <- function(x,
                                           b,
                                           c,
                                           parms,
                                           cons_features,
                                           site_threat_array_cat,
                                           all_run_results,
                                           action_cost_list,
                                           responses_to_actions_EXP,
                                           required_actions,
                                           site_species_array,
                                           output_by_species) {


  # -------------------------------------- start


  ID_exp <- x$ID_exp
  #cat("ID exp =", ID_exp, "\n") #debugging

  ID_run <- x$ID_run
  #cat("ID run =", ID_run, "\n") #debugging

  res_type <- x$response_type
  #cat("response type =", res_type, "\n") #debugging

  responses_to_actions_OBS <- b[[res_type]]
  OBS_response_tag <- c[[res_type]]

  no.sites <- nrow(site_threat_array_cat)
  no.species <- nrow(cons_features)
  no.actions <- ncol(site_threat_array_cat)

  # get site action array from the best run for a particular estimate and target level combination
  selected_sites_and_actions <- all_run_results[[ID_run]]$site_action_array

  species_targets <- all_run_results[[ID_run]]$targets
  #cat("individual species targets = ", species_targets, "\n") #debugging

  # create list for storing EXPECTED response values (best guess)
  SpeciesCount_list_EXP <- lapply(1:no.sites, matrix, data = 0, nrow = no.actions, ncol = no.species)

  # create list for storing OBSERVED response values (lower and upper guess)
  SpeciesCount_list_OBS <- lapply(1:no.sites, matrix, data = 0, nrow = no.actions, ncol = no.species)

  for (site in 1:no.sites)
  {
    #cat("site = ", site, "\n") #debugging

    for (action in 1:no.actions)
    {
      #cat("action = ", action, "\n") #debugging

      # extract EXPECTED response values for each selected site and action in the solution matrix
      run_count_EXP <- count.persistence(
        site,
        action,
        selected_sites_and_actions,
        action_cost_list,
        site_threat_array_cat,
        responses_to_actions_EXP)

      SpeciesCount_list_EXP [[site]][action,] <- run_count_EXP[[2]]

      # extract OBSERVED response values for each selected site and action in the solution matrix
      run_count_OBS <- count.persistence(
        site,
        action,
        selected_sites_and_actions,
        action_cost_list,
        site_threat_array_cat,
        responses_to_actions_OBS)

      SpeciesCount_list_OBS [[site]][action,] <- run_count_OBS[[2]]

    }

  }

  # sum OBSERVED responses across actions
  SpeciesCount_list_sum_OBS <- lapply(SpeciesCount_list_OBS, function(x){apply(x, 2, sum)})

  # convert list to matrix
  SpeciesCount_mat_OBS <- do.call(rbind, SpeciesCount_list_sum_OBS)

  # calculate species representation
  SpeciesBenefit_mat_OBS <- pmin(SpeciesCount_mat_OBS / required_actions, 1)

  # multiply by the area of occurrence of each species
  SpeciesBenefit_mat_OBS <- SpeciesBenefit_mat_OBS * site_species_array

  # sum across sites
  SpeciesBenefit_vec_OBS <- apply(SpeciesBenefit_mat_OBS, 2, sum)

  ## repeat for EXPECTED responses
  SpeciesCount_list_sum_EXP <- lapply(SpeciesCount_list_EXP, function(x){apply(x, 2, sum)})
  SpeciesCount_mat_EXP <- do.call(rbind, SpeciesCount_list_sum_EXP)
  SpeciesBenefit_mat_EXP <- pmin(SpeciesCount_mat_EXP / required_actions, 1)
  SpeciesBenefit_mat_EXP <- SpeciesBenefit_mat_EXP * site_species_array
  SpeciesBenefit_vec_EXP <- apply(SpeciesBenefit_mat_EXP, 2, sum)

  # create output table
  species_error_table <- data.frame(species_id = 1:no.species)

  species_error_table$target <- species_targets
  species_error_table$expected <- SpeciesBenefit_vec_EXP
  species_error_table$observed <- SpeciesBenefit_vec_OBS
  species_error_table$error <- (species_error_table$expected - species_error_table$observed) / species_error_table$expected
  species_error_table$perc_change <- ((species_error_table$observed - species_error_table$expected) / species_error_table$expected) * 100
  species_error_table$shortfall <- ((species_error_table$observed - species_error_table$target) / species_error_table$target) * 100

  if(output_by_species){

    table_name <- sprintf("errors_by_species_run_%s%s", ID_run, ".rds")

    out_pth <- file.path("output", paste("exp", ID_exp, sep = "_"),
                         "uncertainty_analysis",
                         OBS_response_tag)

    write_out_rds(species_error_table, out_pth, table_name)

  }

  species_error_table

}

#' Average uncertainty diagnostics across species.
#'
#' @param the output of \code{calculate_representation_error()}
#'
#' @export
calc_uncertainty_diagnostics <- function(x){

  diagnostics <- c(
    "observed",
    "expected",
    "error",
    "mean_perc_change",
    "sd_mean_perc_change",
    "se_mean_perc_change",
    "prop_below",
    "prop_above",
    "prop_at",
    "Msf_neg",
    "SEsf_neg",
    "Msf_pos",
    "SEsf_pos")

  output_vec <- setNames(rep(0, length(diagnostics)), diagnostics)

  no_reps <- nrow(x)

  prop_below <- length(which(x$observed < x$target)) / no_reps
  prop_above <- length(which(x$observed >= x$target)) / no_reps
  prop_at <- length(which(x$observed == x$target)) / no_reps

  # sum errors across species
  error_sums <- colSums(x[, c("observed", "expected", "error")])

  mean_perc_change <- mean(x$perc_change)
  sd_mean_perc_change <- sd(x$perc_change)
  se_mean_perc_change <- sd_mean_perc_change / sqrt(length(x$perc_change))

  neg_sf_ids <- which(x$shortfall < 0)
  pos_sf_ids <- which(x$shortfall > 0)

  mean_neg_sf <- mean(x$shortfall[neg_sf_ids])
  sd_m_neg_sf <- sd(x$shortfall[neg_sf_ids])
  se_m_neg_sf <- sd_m_neg_sf / sqrt(length(x$shortfall[neg_sf_ids]))

  mean_pos_sf <- mean(x$shortfall[pos_sf_ids])
  sd_m_pos_sf <- sd(x$shortfall[pos_sf_ids])
  se_m_pos_sf <- sd_m_pos_sf / sqrt(length(x$shortfall[pos_sf_ids]))

  output_vec[] <- c(error_sums[1],
                    error_sums[2],
                    error_sums[3],
                    mean_perc_change,
                    sd_mean_perc_change,
                    se_mean_perc_change,
                    prop_below,
                    prop_above,
                    prop_at,
                    mean_neg_sf,
                    se_m_neg_sf,
                    mean_pos_sf,
                    se_m_pos_sf)

  output_vec
}
