#target area of occupancy = 1000, best guess
species_persistence_levels <- all_run_results_exp_8[[10]][[8]]/site_species_array 
species_persistence_levels[is.na(species_persistence_levels)] <- 0
species_persistence_levels[species_persistence_levels==0] <- NA
tiff(file.path("figures", "figures_exp_8", "species_persistence_plot_1.tiff"), 
     width = 10, height = 8, units = "in", pointsize = 12,
     compression = "lzw",
     bg = "white", res = 96)
hist(unlist(species_persistence_levels),100,main="target = 1000 km2 \n response estimate used = best guess",
     xlab="probability of persistence of species in selected planning units")
dev.off()


#target area of occupancy = 1000, lower bound
species_persistence_levels <- all_run_results_exp_8[[11]][[8]]/site_species_array 
species_persistence_levels[is.na(species_persistence_levels)] <- 0
species_persistence_levels[species_persistence_levels==0] <- NA
tiff(file.path("figures", "figures_exp_8", "species_persistence_plot_2.tiff"), 
     width = 10, height = 8, units = "in", pointsize = 12,
     compression = "lzw",
     bg = "white", res = 96)
hist(unlist(species_persistence_levels),100,main="target = 1000 km2 \n response estimate used = lower bound",
     xlab="probability of persistence of species in selected planning units")
dev.off()

#target area of occupancy = 1000, upper bound
species_persistence_levels <- all_run_results_exp_8[[12]][[8]]/site_species_array 
species_persistence_levels[is.na(species_persistence_levels)] <- 0
species_persistence_levels[species_persistence_levels==0] <- NA
tiff(file.path("figures", "figures_exp_8", "species_persistence_plot_3.tiff"), 
     width = 10, height = 8, units = "in", pointsize = 12,
     compression = "lzw",
     bg = "white", res = 96)
hist(unlist(species_persistence_levels),100,main="target = 1000 km2 \n response estimate used = upper bound",
     xlab="probability of persistence of species in selected planning units")
dev.off()
