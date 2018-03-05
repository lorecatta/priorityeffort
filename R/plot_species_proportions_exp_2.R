#' Create and save a scatter plot of the proportion of species above and below
#'  target for different target levels.
#'
#' @param aa the dataframe with the data to plot.
#' @param parms a list of parameters.
#'
#' @export
plot_species_proportions <- function(aa, parms){

  out_pth <- file.path("figures", paste("exp", parms$Exp, sep="_"))

  dir.create(out_pth, FALSE, TRUE)

  plot.file.name <- "Prop_species_above_below_on_target.png"

  png(file.path(out_pth, plot.file.name),
      width = 14,
      height = 12,
      units = "cm",
      res = 300)

  p <- ggplot(aa, aes_string(x = "target_level", y = "value", shape = "response_type")) +
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
    theme(axis.text = element_text(size = 10),
          axis.title.x = element_text(size = 11),
          axis.title.y = element_text(size = 11),
          plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
          legend.key.size = unit(0.5, "cm"),
          legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8))

  print(p)

  dev.off()

}
