plot_change_from_target <- function(aa, parms){

  out_pth <- file.path("figures", paste("exp", parms$Exp, sep="_"))

  dir.create(out_pth, FALSE, TRUE)

  plot.file.name <- "perc_change_from_target.png"

  png(file.path(out_pth, plot.file.name),
      width = 8,
      height = 8,
      units = "cm",
      res = 300)

  p <- ggplot(aa) +
    aes(x = target_level, y = Msf, colour = response_type) +
    geom_point(size = 1) +
    geom_errorbar(aes(ymin = Msf - SEsf, ymax = Msf + SEsf),
                  width = .15) +
    facet_wrap(~ time, scales = "free", nrow = 2) +
    labs(colour = "True response") +
    scale_x_continuous("Target level") +
    scale_y_continuous("% change from target") +
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
