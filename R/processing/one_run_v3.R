one.run <- function(x, cons_feat_array, all_site_action_int_combs, action_costs, site_threat_array_cat, site_species_array, species_responses, all_upstream_connections, boundary.file, all_downstream_connections)
{

  #browser()

  ID_exp <- x$ID_exp
  cat("ID experiment =", ID_exp, "\n")

  ID_run <- x$ID_run
  cat("ID run =", ID_run, "\n")

  CP <- x$CP
  cat("CSM =", CP, "\n")

  estimate <- x$estimate
  cat("expert response =", estimate, "\n")

  target_level <- x$target_level
  cat("target level =", target_level, "\n")

  responses_to_actions <- get.responses.to.actions (species_responses, cons_feat_array, estimate)
  responses_to_actions.best_guesses <- get.responses.to.actions (species_responses, cons_feat_array, 1)
  required_actions <- get.required_actions (site_threat_array_cat, responses_to_actions.best_guesses, cons_feat_array)

  site_action_array <- site_threat_array_cat
  site_action_array[] <- 0

  # set targets
  cons_feat_array <- set_fixed_targets(cons_feat_array, site_species_array, target_level)

  # run the optimization
  run <- Optimize(
    cons_feat_array,
    all_site_action_int_combs,
    site_action_array,
    action_costs,
    site_threat_array_cat,
    responses_to_actions,
    site_species_array,
    all_upstream_connections,
    boundary.file,
    required_actions,
    CP,
    all_downstream_connections)

  initial_cost <- as.vector(run[[1]]["CostCount"])
  ann_summary <- run[[2]]
  site_action_array <- run[[3]]
  SpeciesBenefit_mat <- run[[4]]
  final_species_benefit <- sum(SpeciesBenefit_mat)

  final_cost <- as.vector(ann_summary[nrow(ann_summary),"CostCount"])
  final_species_penalty <- as.vector(ann_summary[nrow(ann_summary),"PenaltyCount"])
  final_conn_penalty <- as.vector(ann_summary[nrow(ann_summary),"ConnPenalty"])
  final_obj_fun_value <- as.vector(ann_summary[nrow(ann_summary),"OF"])

  no_PUs <- length(which(apply(site_action_array, 1, function(x) min(sum(x),1))==1))

  # plot temperature, costs and penalties
  plot.optimize.output(ann_summary, ID_exp, ID_run)

  # write out the annealing summary
  #dir.create(file.path("output", paste("output_exp", ID_exp, sep="_"), "annealing_summaries"), FALSE, TRUE)
  #annealing_summary_name <- sprintf("annealing_summary_%s%s", paste("run", ID_run, sep="_"), ".rds")
  #saveRDS(ann_summary, file.path("output", paste("output_exp", ID_exp, sep="_"), "annealing_summaries", annealing_summary_name))

  # write out the site_action_array
  #dir.create(file.path("output", paste("output_exp", ID_exp, sep="_"), "best_solutions"), FALSE, TRUE)
  #site_action_array_name <- sprintf("site_action_array_%s%s", paste("run", ID_run, sep="_"), ".rds")
  #saveRDS(site_action_array, file.path("output", paste("output_exp", ID_exp, sep="_"), "best_solutions", site_action_array_name))

  # record outputs
  output <- vector("list", 10)
  output [[1]] <- initial_cost
  output [[2]] <- final_cost                  # needed
  output [[3]] <- final_species_benefit       # needed
  output [[4]] <- final_species_penalty       # needed
  output [[5]] <- final_conn_penalty          # needed
  output [[6]] <- final_obj_fun_value         # needed
  output [[7]] <- site_action_array           # needed
  output [[8]] <- SpeciesBenefit_mat
  output [[9]] <- no_PUs                      # needed
  output [[10]] <- cons_feat_array[,"target"] # needed

  # save R workspace
  #dir.create(file.path("output", paste("output_exp", ID_exp, sep="_"), "R_workspaces"), FALSE, TRUE)
  #workspace_name <- sprintf("Run_%s%s", ID_run, ".RData")
  #save(output, file = file.path("output", paste("output_exp", ID_exp, sep="_"), "R_workspaces", workspace_name))

  output
}
