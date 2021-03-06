#' Create and save a bar plot of feature representation error.
#'
#' @param aa the dataframe with the data to plot.
#' @param parms a list of parameters.
#'
#' @export
bar_plot_representation_error <- function(aa, parms){

  #browser()

  out_pth <- file.path("figures", paste("exp", parms$Exp, sep="_"))

  dir.create(out_pth, FALSE, TRUE)

  plot.file.name <- "Representation_error.png"

  y_values <- pretty(aa$mean_perc_change, n = 10)

  png(file.path(out_pth, plot.file.name),
      width = 8,
      height = 8,
      units = "cm",
      res = 600)

  p <- ggplot(aa, aes(x = response_type, y = mean_perc_change)) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_errorbar(aes(ymin = mean_perc_change - se_mean_perc_change,
                      ymax = mean_perc_change + se_mean_perc_change),
                  width = .15) +
    #labs(colour = "True response") +
    geom_abline(intercept = 0, slope = 0, linetype = 1) +
    scale_x_discrete("True response", labels = c("Best guess",
                                                 "Lower bound",
                                                 "Upper bound")) +
    scale_y_continuous("Change in species representation") +
    theme(axis.text.x = element_text(size = 8, angle = 0, vjust = 1, hjust = 0.5),
          axis.title.x = element_text(vjust = 0.5, margin = margin(t = 10)),
          axis.title.y = element_text(vjust = 0.5),
          plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))

  print(p)

  dev.off()

}
