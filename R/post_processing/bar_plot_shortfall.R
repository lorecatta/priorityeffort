bar_plot_shortfall <- function(aa, parms, analysis){
  
  plot.file.name <- paste0("Prop_species_above_below_on_target_exp_", 
                           parms$Exp, 
                           "_",
                           analysis, 
                           ".tiff")
  
  tiff(filename = file.path("figures", 
                            paste("figures_exp", parms$Exp, sep = "_"), 
                            "uncertainty_analysis",
                            plot.file.name), 
       width = 7, 
       height = 3, 
       units = "in", 
       compression = "lzw", 
       res = 300)
  
  p <- ggplot(aa, aes(x = response_type, y = Msf)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = Msf - SEsf, ymax = Msf + SEsf), 
                  width = .3) + 
    facet_wrap(~ time, ncol = 3) +
    scale_x_discrete("", labels = c("Best guess", "Lower bound", "Upper bound")) +
    scale_y_continuous("% change from target",
                       limits = c(0, 1),
                       breaks = seq(0, 1, 0.2)) +
    theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
          axis.title.x = element_text(vjust = 0.5),
          axis.title.y = element_text(vjust = 0.5)) 
  
  print(p)
  
  dev.off()
  
}
