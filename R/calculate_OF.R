components.OF <- function(cons_feat_array,
                          all_site_action_int_combs,
                          site_action_array,
                          action_costs,
                          site_threat_array_cat,
                          responses_to_actions,
                          site_species_array,
                          all_upstream_connections,
                          boundary.file,
                          required_actions,
                          C.P,
                          Start_prop = 0.2){

  #define variables within the function
  no.sites <- nrow(site_action_array)
  no.species <- nrow(cons_feat_array)
  no.actions <- ncol(site_action_array)
  Target <- cons_feat_array[,"target"]
  S.P.F <- cons_feat_array[,"spf"]

  #create list for recording probabilty of persistence
  SpeciesCount_list <- lapply(1:no.sites, matrix, data=0, nrow=no.actions, ncol=no.species)

  #create matrix for counting costs
  CostCount_mat <- matrix(0, nrow=no.sites, ncol=no.actions)

  #get unique combinations of sites and actions
  all_site_action_combs <- aggregate(all_site_action_int_combs[,c(1,2)], list(all_site_action_int_combs[,"action"], all_site_action_int_combs[,"site"]), mean)

  #initial allocation of intensities to actions within sites
  for (i in 1:nrow(all_site_action_combs))
  {

    rnd_number <- runif(n=1, min=0, max=1)

    if (rnd_number < Start_prop)
    {

      site <- all_site_action_combs[i,"site"]
      action <- all_site_action_combs[i,"action"]

      available_action_intensities <- all_site_action_int_combs[which(all_site_action_int_combs[,"site"]==site & all_site_action_int_combs[,"action"]==action), "intensity"]

      resample <- function(x,n) x[sample.int(length(x),n)]
      action_intensity <- resample(available_action_intensities, 1)
      site_action_array [site,action] <- action_intensity

    }

  }

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

    #get the connections upstream of the selected site
    Upstream_connections <- all_upstream_connections[[site]]

    #get the sites upstream of the selected site
    Upstream.Sites <- boundary.file[Upstream_connections,"id2"]

    if (length(Upstream.Sites)!=0)
    {

      #is there at least one action/intensity combination implemented upstream
      upstream_implemented_actions <- apply(site_action_array[Upstream.Sites,,drop=F], 1, function(x) min(sum(x),1))

      #is there at least one action/intensity combination implemented in the selected site
      site_implemented_actions <- min(sum(site_action_array[site,,drop=F]),1)

      #create a vector of penalties for upstream connection
      upstream_conn_penalty_presence <- rep(0, length(Upstream.Sites))

      #if there is at least one action/intensity combination implemented in the selected site
      if (site_implemented_actions == 1)
      {

        #add a connectivity penalty for each connection where no action (including reservation) is implemented in the upstream sites
        upstream_conn_penalty_presence [upstream_implemented_actions == 0] <- 1

      }

      #update boundary file
      boundary.file[Upstream_connections, "penalty"] <- upstream_conn_penalty_presence

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

  #calculate connectivity penalty
  ConnPenalty <- sum(boundary.file[, "boundary"] * boundary.file[, "penalty"])

  #calculate value of the objective function
  OF_value <- CostCount + sum(S.P.F * PenaltyCount_vec) + C.P * ConnPenalty

  #create empty list of outputs
  list_of_outputs <- vector("list", 7)

  #fill the list of outputs
  list_of_outputs [[1]] <- c(CostCount=CostCount, PenaltyCount=PenaltyCount, ConnPenalty=ConnPenalty, OF_value=OF_value)
  list_of_outputs [[2]] <- SpeciesBenefit
  list_of_outputs [[3]] <- site_action_array
  list_of_outputs [[4]] <- SpeciesCount_list
  list_of_outputs [[5]] <- SpeciesBenefit_mat
  list_of_outputs [[6]] <- CostCount_mat
  list_of_outputs [[7]] <- boundary.file

  #return output
  list_of_outputs

}
