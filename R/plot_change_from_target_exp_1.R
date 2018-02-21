bar_plot_change_from_target <- function(aa, parms) {

  out_pth <- file.path("figures", paste("exp", parms$Exp, sep="_"))

  dir.create(out_pth, FALSE, TRUE)

  plot.file.name <- "perc_change_from_target.png"

  png(file.path(out_pth, plot.file.name),
       width = 7,
       height = 3,
       units = "in",
       res = 300)

  neg <- ggplot(aa[aa$time=="Below target",]) +
    aes(x = response_type, y = Msf) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_errorbar(aes(ymin = Msf - SEsf, ymax = Msf + SEsf),
                  width = .15) +
    facet_wrap(~ time) +
    scale_x_discrete("", labels = c("Best guess", "Lower bound", "Upper bound")) +
    scale_y_continuous("% change from target",
                       limits = c(-20, 0),
                       breaks = seq(-20,0,5)) +
    theme(axis.title.x = element_text(vjust = 0.5),
          axis.title.y = element_text(vjust = 0.5))

  pos <- ggplot(aa[aa$time=="Above target",]) +
    aes(x = response_type, y = Msf) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_errorbar(aes(ymin = Msf - SEsf, ymax = Msf + SEsf),
                  width = .15) +
    facet_wrap(~ time) +
    scale_x_discrete("", labels = c("Best guess", "Lower bound", "Upper bound")) +
    scale_y_continuous("% change from target",
                       limits = c(0, 280),
                       breaks = c(0, 50, 100, 150, 200, 250)) +
    theme(axis.title.x = element_text(vjust = 0.5),
          axis.title.y = element_text(vjust = 0.5))

  p <- gridExtra::grid.arrange(neg, pos, ncol = 2)

  print(p)

  dev.off()

}
