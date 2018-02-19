Optimize <- function(parameters,
                     cons_feat_array,
                     all_site_action_int_combs,
                     site_action_array,
                     action_costs,
                     site_threat_array_cat,
                     responses_to_actions,
                     site_species_array,
                     required_actions){

  #define variables within the function
  no.species <- nrow(cons_feat_array)
  no.actions <- ncol(site_action_array)
  Target <- cons_feat_array[,"target"]
  S.P.F <- cons_feat_array[,"spf"]

  no_ITER <- parameters$no_ITER
  Temp_zero <- parameters$Temp_zero
  cooling_factor <- parameters$cooling_factor
  print_iter <- parameters$print_every_iter

  #set temperature for first cooling level
  Temp <- Temp_zero

  #create empty matrix for output
  mat_headings <- c("Iter", "Site", "Action", "Intensity", "Move", "OF", "CC", "CostCount", "PC",
                    "PenaltyCount", "delta_OF", "choice", "Temp", "testval", "rnd_value")

  Out_mat <- matrix(0, nrow = no_ITER/print_iter, ncol = length(mat_headings))
  colnames(Out_mat) <- 1:dim(Out_mat)[2]
  colnames(Out_mat) <- mat_headings

  list_of_outputs <- vector("list", 4)

  #calculate the initial components of the objective function
  initial_solution <- components.OF(cons_feat_array,
                                    all_site_action_int_combs,
                                    site_action_array,
                                    action_costs,
                                    site_threat_array_cat,
                                    responses_to_actions,
                                    site_species_array,
                                    required_actions)

  #get the outputs
  CostCount <- initial_solution[[1]]["CostCount"]
  PenaltyCount <- initial_solution[[1]]["PenaltyCount"]
  OF_value <- initial_solution[[1]]["OF_value"]

  SpeciesBenefit <- initial_solution[[2]]
  site_action_array <- initial_solution[[3]]
  SpeciesCount_list <- initial_solution[[4]]
  SpeciesBenefit_mat <- initial_solution[[5]]
  CostCount_mat <- initial_solution[[6]]

  #loop!
  for (IT in 1:no_ITER) {

    #cat("iteration =", IT, "\n") #debugging

    site_action_array_1 <- site_action_array
    CostCount_1 <- CostCount
    SpeciesBenefit_1 <- SpeciesBenefit
    PenaltyCount_1 <- PenaltyCount
    OF_value_1 <- OF_value
    SpeciesCount_list_1 <- SpeciesCount_list
    SpeciesBenefit_mat_1 <- SpeciesBenefit_mat
    CostCount_mat_1 <- CostCount_mat

    #sample randomly one combination of site/action/intensity
    rnd_site_action_comb <- sample(x = 1:nrow(all_site_action_int_combs), size = 1)

    #get site
    rnd_site <- as.numeric(all_site_action_int_combs[rnd_site_action_comb, "site"])
    #cat("rnd_site =", rnd_site, "\n") #debugging

    #get action
    rnd_action <- as.numeric(all_site_action_int_combs[rnd_site_action_comb, "action"])
    #cat("rnd_action =", rnd_action, "\n") #debugging

    #get intensity
    rnd_intensity <- as.numeric(all_site_action_int_combs[rnd_site_action_comb, "intensity"])
    #cat("rnd_intensity =", rnd_intensity, "\n") #debugging

    a_i <- rnd_intensity

    #if the selected intensity is already implemented
    if(site_action_array[rnd_site, rnd_action] == rnd_intensity) {

      #switch it OFF
      site_action_array[rnd_site,rnd_action] <- 0
      a_A_R <- 0


    }else{

      #switch it ON
      site_action_array[rnd_site,rnd_action] <- rnd_intensity
      a_A_R <- 1


    }

    run_count <- count.persistence(rnd_site,
                                   rnd_action,
                                   site_action_array,
                                   action_costs,
                                   site_threat_array_cat,
                                   responses_to_actions)

    CostCount_mat [rnd_site,rnd_action] <- run_count[[1]]
    SpeciesCount_list [[rnd_site]][rnd_action,] <- run_count[[2]]

    SpeciesCount_vec <- apply(SpeciesCount_list [[rnd_site]], 2, sum)

    #calculate species benefit at rnd site
    SpeciesBenefit_mat[rnd_site, ] <- pmin(SpeciesCount_vec / required_actions[rnd_site, ], 1) * site_species_array[rnd_site, ]

    ### start repeated sub

    #calculate total species benefit
    SpeciesBenefit <- apply(SpeciesBenefit_mat, 2, sum)
    #cat("SpeciesBenefit =", SpeciesBenefit, "\n") #debugging

    #calculate penalty count
    PenaltyCount_vec <- pmax.int(Target - SpeciesBenefit, 0)
    PenaltyCount <- sum(PenaltyCount_vec)

    #calculate total cost for all sites
    CostCount <- sum(CostCount_mat)

    ### end repeated sub

    OF_value <- CostCount + sum(S.P.F * PenaltyCount_vec)

    #calculate delta
    delta_OF <- OF_value - as.vector(OF_value_1)

    if(IT %% print_iter==0) {

      IT_to_print <- IT/print_iter

      Out_mat [IT_to_print,"CC"] <- CostCount
      Out_mat [IT_to_print,"PC"] <- PenaltyCount

    }

    #accept bad move?
    if(delta_OF <= 0) {

      good_choice <- 1

      testval <- NA
      rnd_value <- NA

    } else {

      testval <- exp(-delta_OF / Temp)

      rnd_value <- runif(n = 1, min = 0, max = 1)

      if(testval > rnd_value & testval != 1) {

        good_choice <- 1

      } else {

        good_choice <- 0

        site_action_array <- site_action_array_1
        CostCount <- CostCount_1
        SpeciesBenefit <- SpeciesBenefit_1
        PenaltyCount <- PenaltyCount_1
        OF_value <- OF_value_1
        SpeciesCount_list <- SpeciesCount_list_1
        SpeciesBenefit_mat <- SpeciesBenefit_mat_1
        CostCount_mat <- CostCount_mat_1

      }

    }

    if(IT %% print_iter == 0){

      IT_to_print <- IT/print_iter

      Out_mat [IT_to_print,"Iter"] <- IT
      Out_mat [IT_to_print,"Site"] <- rnd_site
      Out_mat [IT_to_print,"Action"] <- rnd_action
      Out_mat [IT_to_print,"Intensity"] <- a_i
      Out_mat [IT_to_print,"Move"] <- a_A_R
      Out_mat [IT_to_print,"OF"] <- OF_value
      Out_mat [IT_to_print,"CostCount"] <- CostCount
      Out_mat [IT_to_print,"PenaltyCount"] <- PenaltyCount
      Out_mat [IT_to_print,"delta_OF"] <- delta_OF
      Out_mat [IT_to_print,"choice"] <- good_choice
      Out_mat [IT_to_print,"Temp"] <- Temp
      Out_mat [IT_to_print,"testval"] <- testval
      Out_mat [IT_to_print,"rnd_value"] <- rnd_value

    }

    #update temperature
    Temp <- Temp * cooling_factor

  }

  #fill the list of outputs
  list_of_outputs [[1]] <- initial_solution[[1]]
  list_of_outputs [[2]] <- Out_mat
  list_of_outputs [[3]] <- site_action_array
  list_of_outputs [[4]] <- SpeciesBenefit_mat

  #return the list of outputs
  list_of_outputs
}
