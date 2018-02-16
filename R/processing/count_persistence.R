count.persistence <- function(
  site, action, site_action_array, 
  action_costs, site_threat_array_cat, responses_to_actions){
  
  #cat('site = ', site, '\n') #debugging
  #cat('action = ', action, '\n') #debugging
  
  #define variables within the function
  no.actions <- ncol(site_action_array)
  no.species <- length(responses_to_actions)/no.actions

  output <- vector('list', 2)
  speciesCount <- rep(0,no.species)
  
  #get the level of effort to allocate to the action at the site
  action_intensity <- site_action_array[site,action]  
  #cat('action intensity =', action_intensity, '\n') #debugging
  
  #get cost of action intensity
  costCount <- action_costs[[site]][action_intensity+1,action]
  
  #get the probability of persistence given by the selected action intensity, for each species
  for (z in seq_len(no.species))
  {

    species <- z
    #cat('species =', species, '\n') #debugging    
    
    #get the threat intensity category
    threat_intensity_cat <- site_threat_array_cat[site,action]
    #cat('threat_intensity_cat =', threat_intensity_cat, '\n') #debugging       

    index <- species + (no.species*(action-1))
    #cat('index =', index, '\n') #debugging

    #get the species probability of persistence
    speciesCount[species] <- responses_to_actions[[index]][action_intensity+1,threat_intensity_cat+1] 

  }

  #cat('costCount =', costCount, '\n') #debugging
  #cat('speciesCount =', speciesCount, '\n') #debugging  
  
  output[[1]] <- costCount
  output[[2]] <- speciesCount
  output
}
