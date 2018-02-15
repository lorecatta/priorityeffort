plot_representation_error <- function(aa, parms, analysis, by_var){
  
  dir.create(file.path("figures", paste("figures_exp", parms$Exp, sep="_"), "uncertainty_analysis"), 
             FALSE, TRUE)
  
  plot.file.name <- sprintf("Representation_error_vs_TargetLevel_exp_%s_%s%s", parms$Exp, analysis, ".tiff")
  
  my_labs <- as_labeller(c(`Species below target` = "Below target", 
                           `Species above target` = "Above target"))

    #se_values <- aa$se_mean_perc_change
  y_values <- pretty(aa$mean_perc_change, n = 10)
  x_values <- pretty(aa$target_level, n = 10)
  
  tiff(filename = file.path("figures", paste("figures_exp", parms$Exp, sep = "_"), 
                            "uncertainty_analysis",
                            plot.file.name), 
       width = 6, height = 4, units = "in", 
       compression = "lzw", res = 300)
  
  print(
    ggplot(aa, aes(x = target_level, y = mean_perc_change, colour = get(by_var))) +
      geom_errorbar(aes(ymin = mean_perc_change - se_mean_perc_change, ymax = mean_perc_change + se_mean_perc_change), 
                    width = .5) +
      geom_point(size = 1) +
      labs(colour = "True response") +
      geom_abline(intercept = 0, slope = 0, linetype = 2) +
      scale_x_continuous(expression("Target level " ~ (10^{2} ~ "km"^{2})),
                         limits = c(min(x_values), max(x_values)),
                         breaks = x_values,
                         labels = x_values / 100) +
      scale_y_continuous("Change in species representation",
                         limits = c(min(y_values), max(y_values)),
                         breaks = y_values) +
      theme(axis.text.x=element_text(angle = 0, vjust = 1, hjust = 0.5),
            axis.title.x=element_text(vjust = 0.5),
            axis.title.y=element_text(vjust = 0.5),
            plot.margin=unit(c(0.5,0,0.5,0.5), "cm"))
  )
  
  dev.off()
}
