plot_species_proportions <- function(aa, parms, analysis, by_var){

  out_pth <- file.path("figures", paste("exp", parms$Exp, sep="_"))

  dir.create(out_pth, FALSE, TRUE)

  plot.file.name <- "Prop_species_above_below_on_target.png"

  png(file.path(out_pth, plot.file.name),
      width = 8,
      height = 8,
      units = "cm",
      res = 300)

  p <- ggplot(aa, aes_string(x = "target_level", y = "value", shape = by_var)) +
    geom_point(size = 1) +
    facet_wrap(~ species_prop, nrow = 3) +
    labs(shape = "True response") +
    scale_x_continuous(expression("Target level " ~ (10^{2} ~ "km"^{2})),
                       limits = c(0, 10000),
                       breaks = seq(0, 10000, 1000),
                       labels = seq(0, 100, 10)) +
    scale_y_continuous("Proportion of species",
                       limits = c(0, 1),
                       breaks = seq(0, 1, 0.2)) +
    theme(axis.text = element_text(size = 7),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
          legend.key.size = unit(0.3, "cm"),
          legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 6),
          strip.text = element_text(size = 8))

  print(p)

  dev.off()

}