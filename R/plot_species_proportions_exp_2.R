plot_species_proportions <- function(aa, parms, analysis, by_var){
  
  plot.file.name <- paste0("Prop_species_above_below_on_target_exp_", 
                           parms$Exp, 
                           "_",
                           analysis, 
                           ".tiff")
  
  tiff(filename = file.path("figures", 
                            paste("figures_exp", parms$Exp, sep = "_"), 
                            "uncertainty_analysis",
                            plot.file.name), 
       width = 6, height = 6, units = "in", compression = "lzw", res = 300)
  
  print(
    ggplot(aa, aes(x = target_level, y = value, shape = get(by_var))) +
      geom_point(size = 1) +
      facet_wrap(~ species_prop, nrow = 3) +
      ggtitle("") +
      labs(shape = "True response") +
      scale_x_continuous(expression("Target level " ~ (10^{2} ~ "km"^{2})),
                         limits = c(0, 10000),
                         breaks = seq(0, 10000, 1000),
                         labels = seq(0, 100, 10)) +
      scale_y_continuous("Proportion of species",
                         limits = c(0, 1),
                         breaks = seq(0, 1, 0.2)) +
      theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
            axis.title.x = element_text(vjust = 0.5),
            axis.title.y = element_text(vjust = 0.5)) 
  )
  
  dev.off()
  
}
