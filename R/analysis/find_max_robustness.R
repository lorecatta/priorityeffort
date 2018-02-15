no.sites <- 865
no.actions <- 4
no.species <- 138
site_threat_array_cat <- get.threat.intensity.category (parameters, site_threat_array)
action_costs <- get.action.costs (site_threat_array_cat, planning_unit)
species_responses <- cbind(species_responses, alpha_responses=0)

responses_to_actions.best_guesses <- get.responses.to.actions (species_responses, cons_feat_array, 1)
required_actions <- get.required_actions (site_threat_array_cat, responses_to_actions.best_guesses, cons_feat_array)

runs <- exp.des$ID_run 

alpha_values <- c()

for (run in 1:length(runs))
{
  ID <- runs[run]
  cat("Run =", ID, "\n")
  
  cons_feat_array <- set.species.targets (cons_feat_array, site_species_array, exp.des[ID,"target_level"])
  Target <- cons_feat_array[,1]
  site_action_array <- results_exp_14[[ID]][[7]]

  start <- 0
  final <- 1

  for (iter in 1:20)
  {
    cat("iteration =", iter, "\n")
    cat("start =", start, "\n") #debugging
    cat("final =", final, "\n") #debugging
  
    alpha <- (start+final)/2
  
    cat("alpha =", alpha, "\n") #debugging

    new_values <- species_responses[,"PP_BestGuess"] - (species_responses[,"half_bound"] * alpha)

    species_responses[,"alpha_responses"] <- new_values

    responses_to_actions <- get.responses.to.actions (species_responses, cons_feat_array, 4)

    #create list for recording probabilty of persistence
    SpeciesCount_list <- lapply(1:no.sites, matrix, data=0, nrow=no.actions, ncol=no.species)

    #create matrix for counting costs
    CostCount_mat <- matrix(0, nrow=no.sites, ncol=no.actions)

    for (j in seq_len(no.sites))
    {
      site <- j
      #cat("site =", site, "\n") #debugging
      for (action in 1:no.actions)
      {
    
        #cat("action =", action, "\n") #debugging
    
        run_count <- count.persistence(site, action, site_action_array, action_costs, site_threat_array_cat, responses_to_actions)
    
        CostCount_mat [site,action] <- run_count[[1]]
        SpeciesCount_list [[site]][action,] <- run_count[[2]]
    
      }

    }

    SpeciesCount_list_sum <- lapply(SpeciesCount_list, function(x){apply(x, 2, sum)})

    SpeciesCount_mat <- do.call(rbind, SpeciesCount_list_sum)

    #calculate species benefit at each site
    SpeciesBenefit_mat <- (SpeciesCount_mat/required_actions) * site_species_array

    #calculate total species benefit
    SpeciesBenefit <- apply(SpeciesBenefit_mat,2,sum)

    print(all(SpeciesBenefit >= Target))
  
    if(all(SpeciesBenefit >= Target))
    {
      start <- alpha  
    }else{
      final <- alpha  
    }

  }

  alpha_values[run] <- alpha

}

names(alpha_values) <- exp.des$target_level
plot(alpha_values, exp.des$target_level)
#segments(0, exp.des$target_level, alpha_values, 0)
