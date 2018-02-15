library(ggplot2)

cost_dataset <- data.frame(level = factor(c("Low","Medium","High"), levels=c("Low","Medium","High")), cost = c(0.3,0.6,0.9))

ggplot(cost_dataset, aes(x = level, y = cost)) + 
  geom_point(size=6) +
  ggtitle("") +
  xlab("Management effort") +
  ylab("Cost of action") +
  scale_y_continuous(limits=c(0, 1)) + 
  theme(plot.title = element_text(size = 30, face = "bold"),
        axis.line.x = element_line(colour = "black", size = 1),
        axis.line.y = element_line(colour = "black", size = 1),
        axis.text = element_text(size = 25, face="bold"),
        axis.title.y = element_text(size = 30, face = "bold", margin = margin(0,20,0,0)),
        axis.title.x = element_text(size = 30, face = "bold", margin = margin(20,0,0,0)))
ggsave(filename = file.path("figures", "action_cost_example.tiff"),
       width = 10, height = 6, units = "in", dpi = 200, compression = "lzw")
