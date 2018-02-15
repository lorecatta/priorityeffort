bar_plot_species_proportions <- function(aa, parms, analysis, by_var){
  
  plot.file.name <- paste0("Prop_species_above_below_on_target_exp_", 
                           parms$Exp, 
                           "_",
                           analysis, 
                           ".tiff")
  
  my_labs <- as_labeller(c(`Species below target` = "Below target", 
                           `Species above target` = "Above target"))
  
  tiff(filename = file.path("figures", 
                            paste("figures_exp", parms$Exp, sep = "_"), 
                            "uncertainty_analysis",
                            plot.file.name), 
       width = 7, 
       height = 3, 
       units = "in", 
       compression = "lzw", 
       res = 300)
  
  p <- ggplot(aa, aes(x = response_type, y = value)) +
    geom_bar(stat = "identity", width = 0.5) +
    facet_wrap(~ species_prop, ncol = 3, labeller = labeller(species_prop = my_labs)) +
    scale_x_discrete("", labels = c("Best guess", "Lower bound", "Upper bound")) + 
    scale_y_continuous("Proportion of species",
                       limits = c(0, 1),
                       breaks = seq(0, 1, 0.2)) +
    theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
          axis.title.x = element_text(vjust = 0.5),
          axis.title.y = element_text(vjust = 0.5)) 
  
  print(p)
  
  dev.off()
  
}
