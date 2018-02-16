
library(ggplot2)

targets <- read.csv(file.path("output", "output_exp_18", "species_targets.csv"))

ggplot(targets, aes(x = area_of_occ / 100, y = target_perc)) + 
  geom_line(size = 1.3) +
  scale_x_continuous(expression("Area of occupancy " ~ (10^{2} ~ "km"^{2})), 
                     breaks = seq(0, 500, 100),
                     labels = seq(0, 500, 100),
                     limits = c(0, 525)) +
  scale_y_continuous("Species' representation targets", 
                     breaks = seq(0,100,20),
                     labels = paste0(seq(0,100,20), "%")) +
  theme(panel.background = element_rect(size = 1),
        plot.title = element_text(size = 20, face = "bold"),
        axis.line.x = element_line(colour = "black", size = 1),
        axis.line.y = element_line(colour = "black", size = 1),
        axis.text = element_text(size = 15, face="bold"),
        axis.title.y = element_text(size = 20, face = "bold", margin = margin(0,20,0,0)),
        axis.title.x = element_text(size = 20, face = "bold", margin = margin(20,0,0,0)))

ggsave(file.path("figures", "target_linear_interpolation.tiff"),
       width = 6, 
       height = 5.5, 
       units = "in", 
       dpi = 200, 
       compression = "lzw")
