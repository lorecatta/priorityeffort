plot_representation_error <- function(aa, parms, analysis, by_var){

  out_pth <- file.path("figures", paste("exp", parms$Exp, sep="_"))

  dir.create(out_pth, FALSE, TRUE)

  plot.file.name <- "Representation_error.png"

  my_labs <- as_labeller(c(`Species below target` = "Below target",
                           `Species above target` = "Above target"))

  y_values <- pretty(aa$mean_perc_change, n = 5)
  x_values <- pretty(aa$target_level, n = 10)

  png(file.path(out_pth, plot.file.name),
      width = 8,
      height = 7,
      units = "cm",
      res = 300)

  p <- ggplot(aa, aes_string(x = "target_level", y = "mean_perc_change", colour = by_var)) +
    geom_errorbar(aes(ymin = mean_perc_change - se_mean_perc_change,
                      ymax = mean_perc_change + se_mean_perc_change),
                  width = .5) +
    geom_abline(intercept = 0, slope = 0, linetype = 2) +
    geom_point(size = 1) +
    labs(colour = "True response") +
    scale_x_continuous(expression("Target level " ~ (10^{2} ~ "km"^{2})),
                       limits = c(min(x_values), max(x_values)),
                       breaks = x_values,
                       labels = x_values / 100) +
    scale_y_continuous("Change in species representation",
                       limits = c(min(y_values), max(y_values)),
                       breaks = y_values) +
    theme(axis.text = element_text(size = 7),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
          legend.key.size = unit(0.3, "cm"),
          legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 6))

  print(p)

  dev.off()

}
