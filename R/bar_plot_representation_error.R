bar_plot_representation_error <- function(aa, parms, analysis, by_var){
  
  #browser()
  
  dir.create(file.path("figures", paste("figures_exp", parms$Exp, sep="_"), "uncertainty_analysis"), 
             FALSE, TRUE)
  
  plot.file.name <- sprintf("Representation_error_exp_%s_%s%s", parms$Exp, analysis, ".tiff")
  
  #se_values <- aa$se_mean_perc_change
  y_values <- pretty(aa$mean_perc_change, n = 10)
  #x_values <- pretty(aa$target_level, n = 10)
  
  tiff(file.path("figures", paste("figures_exp", parms$Exp, sep = "_"), 
                 "uncertainty_analysis",
                 plot.file.name), 
       width = 4, 
       height = 4, 
       units = "in", 
       compression = "lzw", 
       res = 300)
  
  p <- ggplot(aa, aes(x = response_type, y = mean_perc_change)) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_errorbar(aes(ymin = mean_perc_change - se_mean_perc_change, ymax = mean_perc_change + se_mean_perc_change), 
                  width = .15) +
    #labs(colour = "True response") +
    geom_abline(intercept = 0, slope = 0, linetype = 1) +
    scale_x_discrete("True response", labels = c("Best guess", "Lower bound", "Upper bound")) +
    scale_y_continuous("Change in species representation") +
    theme(axis.text.x=element_text(angle = 0, vjust = 1, hjust = 0.5),
          axis.title.x=element_text(vjust = 0.5),
          axis.title.y=element_text(vjust = 0.5),
          plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"))
  
  print(p)
  
  dev.off()
}
