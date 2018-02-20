wrapper <- function(parms,
                    exp_des,
                    site_threat_array,
                    planning_unit,
                    cons_feat_array,
                    site_species_array,
                    species_responses,
                    parallel){

  fixed_targets <- parms$fixed_targets

  occurrence_limits <- parms$occurrence_limits

  target_limits <- parms$target_limits

  if(!fixed_targets){

    cons_feat_array <- set_scaled_targets(cons_feat_array,
                                          occurrence_limits,
                                          target_limits)
  }

  exp_des_ls <- df_to_list(exp_des, use_names = TRUE)

  if(parallel){

    n_cores <- parallel::detectCores() - 1

    context::parallel_cluster_start(n_cores, ctx)

  }

  out <- loop(exp_des_ls,
              one_run,
              parms,
              site_threat_array,
              planning_unit,
              cons_feat_array,
              site_species_array,
              species_responses,
              parallel = parallel)

  if(parallel){

    context::parallel_cluster_stop()

  }

  out

}

one_run <- function(x,
                    parms,
                    site_threat_array,
                    planning_unit,
                    cons_feat_array,
                    site_species_array,
                    species_responses) {

  #browser()

  fixed_targets <- parms$fixed_targets

  ID_exp <- x$ID_exp
  cat("ID experiment =", ID_exp, "\n")

  ID_run <- x$ID_run
  cat("ID run =", ID_run, "\n")

  estimate <- 1 # at the moment we use experts' best estimates for the prioritization

  site_threat_array_cat <- get.threat.intensity.category(parms, site_threat_array)
  all_site_action_int_combs <- get.site.action.intensities.combs(parms, site_threat_array_cat)
  action_costs <- get.action.costs(site_threat_array_cat, planning_unit)
  responses_to_actions <- get.responses.to.actions(species_responses, cons_feat_array, estimate)
  responses_to_actions.best_guesses <- get.responses.to.actions(species_responses, cons_feat_array, estimate = 1)
  required_actions <- get.required_actions(site_threat_array_cat, responses_to_actions.best_guesses, cons_feat_array)

  site_action_array <- site_threat_array_cat
  site_action_array[] <- 0

  if(fixed_targets){

    target_level <- x$target_level
    cat("target level =", target_level, "\n")

    cons_feat_array <- set_fixed_targets(cons_feat_array, site_species_array, target_level)

  }

  spf_values <- parms$spf

  cons_feat_array[, "spf"] <- spf_values

  # run the optimization
  run <- Optimize(parms,
                  cons_feat_array,
                  all_site_action_int_combs,
                  site_action_array,
                  action_costs,
                  site_threat_array_cat,
                  responses_to_actions,
                  site_species_array,
                  required_actions)

  ann_summary <- run[[2]]
  site_action_array <- run[[3]]
  SpeciesBenefit_mat <- run[[4]]
  final_species_benefit <- sum(SpeciesBenefit_mat)

  final_cost <- as.vector(ann_summary[nrow(ann_summary), "CostCount"])
  final_species_penalty <- as.vector(ann_summary[nrow(ann_summary), "PenaltyCount"])
  final_obj_fun_value <- as.vector(ann_summary[nrow(ann_summary), "OF"])

  no_PUs <- length(which(apply(site_action_array, 1, function(x) min(sum(x),1))==1))

  # # write out the annealing summary
  # out_pth <- file.path("output", paste("exp", ID_exp, sep="_"), "annealing_summaries")
  # dir.create(out_pth, FALSE, TRUE)
  # annealing_summary_name <- sprintf("annealing_summary_%s%s", paste("run", ID_run, sep="_"), ".rds")
  # saveRDS(ann_summary, file.path(out_pth, annealing_summary_name))

  output <- list()

  output$exp <- ID_exp
  output$run <- ID_run
  output$cost <- final_cost                             # needed
  output$species_benefit <- final_species_benefit       # needed
  output$species_penalty <- final_species_penalty       # needed
  output$OF_value <- final_obj_fun_value                # needed
  output$site_action_array <- site_action_array         # needed
  output$species_benefit_array <- SpeciesBenefit_mat
  output$no_pu <- no_PUs                                # needed
  output$targets <- cons_feat_array[,"target"]          # needed
  output$summary <- ann_summary                         # needed

  output
}
