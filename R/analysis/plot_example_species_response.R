library(ggplot2)

dummy_data_set <- data.frame(intensity = factor(c("Low","Medium","High"), 
                                                levels=c("Low","Medium","High")), 
                             pp = c(0.8,0.7,0.3), 
                             ci=c(0.16,0.13,0.2))

ggplot(dummy_data_set, aes(x = intensity, y = pp, ymin = pp-ci, ymax = pp+ci)) + 
    geom_pointrange(size=1) +
    geom_point(size=6) +
    xlab("Threat intensity") +
    scale_y_continuous("Probability of persistence", labels = NULL, limits = c(0, 1)) +
    theme(panel.background = element_rect(size = 1),
          plot.title = element_text(size = 30, face = "bold"),
          axis.line.x = element_line(colour = "black", size = 1),
          axis.line.y = element_line(colour = "black", size = 1),
          axis.text = element_text(size = 20, face="bold"),
          axis.title.y = element_text(size = 25, face = "bold", margin = margin(0,20,0,0)),
          axis.title.x = element_text(size = 25, face = "bold", margin = margin(20,0,0,0)))

ggsave(file.path("figures", "species_response_example.tiff"),
       width = 6, 
       height = 5, 
       units = "in", 
       dpi = 200, 
       compression = "lzw")

ggplot(dummy_data_set, aes(x = intensity, y = pp)) + 
  geom_point(size=6) +
  xlab("Threat intensity") +
  scale_y_continuous("Probability of persistence", labels = NULL, limits = c(0, 1)) +
  theme(panel.background = element_rect(size = 1),
        plot.title = element_text(size = 30, face = "bold"),
        axis.line.x = element_line(colour = "black", size = 1),
        axis.line.y = element_line(colour = "black", size = 1),
        axis.text = element_text(size = 20, face="bold"),
        axis.title.y = element_text(size = 25, face = "bold", margin = margin(0,20,0,0)),
        axis.title.x = element_text(size = 25, face = "bold", margin = margin(20,0,0,0)))

ggsave(file.path("figures", "species_response_example_no_error.tiff"),
       width = 6, 
       height = 5, 
       units = "in", 
       dpi = 200, 
       compression = "lzw")
