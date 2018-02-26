plot_change_from_target <- function(){

  out_pth <- file.path("figures", paste("exp", parms$Exp, sep="_"))

  dir.create(out_pth, FALSE, TRUE)

  plot.file.name <- "perc_change_from_target.png"

  png(file.path(out_pth, plot.file.name),
      width = 8,
      height = 8,
      units = "cm",
      res = 300)

  neg <- ggplot(data_to_plot_long_2[data_to_plot_long_2$time=="Below target",]) +
    aes(x = target_level, y = Msf, colour = response_type) +
    geom_point(size = 1) +
    geom_errorbar(aes(ymin = Msf - SEsf, ymax = Msf + SEsf),
                  width = .15) +
    facet_wrap(~ time) +
    scale_x_continuous() +
    scale_y_continuous("% change from target",
                       limits = c(-28, 0),
                       breaks = seq(-25, 0, 5)) +
    theme(axis.title.x = element_text(vjust = 0.5),
          axis.title.y = element_text(vjust = 0.5))

  pos <- ggplot(data_to_plot_long_2[data_to_plot_long_2$time=="Above target",]) +
    aes(x = target_level, y = Msf, colour = response_type) +
    geom_point(size = 1) +
    geom_errorbar(aes(ymin = Msf - SEsf, ymax = Msf + SEsf),
                  width = .15) +
    facet_wrap(~ time) +
    scale_x_continuous() +
    scale_y_continuous("% change from target",
                       limits = c(0, 1300),
                       breaks = seq(0, 1200, 200)) +
    theme(axis.title.x = element_text(vjust = 0.5),
          axis.title.y = element_text(vjust = 0.5))

  p <- gridExtra::grid.arrange(neg, pos, ncol=2)

  print(p)

  dev.off()

}
