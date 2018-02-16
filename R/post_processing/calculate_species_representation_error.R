###
# function for calculating observed and expected species representation, 
# and commission and omission errors, following Hermoso et al. 2015, Ecography 
###

calculate_representation_error <- function(
  x, b, c, parms, cons_features, 
  site_threat_array_cat.mat, all_run_results, action_cost_list, 
  responses_to_actions_EXP,
  required_actions, site_species_array.mat,
  output_folder, output_by_species) {
  
  
  # -------------------------------------- start
  
  
  ID_exp <- x$ID_exp
  #cat("ID exp =", ID_exp, "\n") #debugging
  
  ID_run <- x$ID_run
  #cat("ID run =", ID_run, "\n") #debugging
  
  res_type <- x$response_type
  #cat("response type =", res_type, "\n") #debugging
  
  responses_to_actions_OBS <- b[[res_type]] 
  OBS_response_tag <- c[[res_type]]
  
  no.sites <- nrow(site_threat_array_cat.mat)
  no.species <- nrow(cons_features)
  no.actions <- ncol(site_threat_array_cat.mat)
  
  # get site action array from the best run for a particular estimate and target level combination
  selected_sites_and_actions <- all_run_results[[ID_run]][[7]]
  
  species_targets <- all_run_results[[ID_run]][[10]]
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
        site_threat_array_cat.mat, 
        responses_to_actions_EXP)
      
      SpeciesCount_list_EXP [[site]][action,] <- run_count_EXP[[2]]        
      
      # extract OBSERVED response values for each selected site and action in the solution matrix
      run_count_OBS <- count.persistence(
        site, 
        action, 
        selected_sites_and_actions, 
        action_cost_list, 
        site_threat_array_cat.mat, 
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
  SpeciesBenefit_mat_OBS <- SpeciesBenefit_mat_OBS * site_species_array.mat
  
  # sum across sites
  SpeciesBenefit_vec_OBS <- apply(SpeciesBenefit_mat_OBS, 2, sum)
  
  ## repeat for EXPECTED responses 
  SpeciesCount_list_sum_EXP <- lapply(SpeciesCount_list_EXP, function(x){apply(x, 2, sum)})
  SpeciesCount_mat_EXP <- do.call(rbind, SpeciesCount_list_sum_EXP)
  SpeciesBenefit_mat_EXP <- pmin(SpeciesCount_mat_EXP / required_actions, 1)
  SpeciesBenefit_mat_EXP <- SpeciesBenefit_mat_EXP * site_species_array.mat
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
    
    table_name <- sprintf("errors_by_species_exp_%s_run_%s_%s%s", ID_exp, ID_run, OBS_response_tag, ".rds")
    
    dir.create(file.path("output", paste("output_exp", ID_exp, sep = "_"), 
                         "uncertainty_analysis",
                         output_folder, 
                         OBS_response_tag), 
               FALSE, TRUE)
    
    saveRDS(species_error_table, file.path("output", paste("output_exp", ID_exp, sep = "_"), 
                                               "uncertainty_analysis",
                                               output_folder, 
                                               OBS_response_tag,
                                               table_name))
    
  }
  
  species_error_table
  
}
