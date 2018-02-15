Rprof("profile.out", memory.profiling = TRUE, line.profiling=TRUE)
test <- apply(exp.des[1,],1, 
              one.run, 
              parameters, 
              cons_feat_array, 
              all_site_action_int_combs, 
              action_costs, 
              site_threat_array_cat, 
              site_species_array, 
              species_responses, 
              all_upstream_connections, 
              boundary.file, 
              all_downstream_connections)
Rprof() 
summaryRprof("profile.out", lines = "show", memory = "both")
