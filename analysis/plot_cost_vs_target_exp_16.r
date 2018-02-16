# load R functions
source(file.path("R", "pre_processing", "parameters_exp_16.R"))


# ---------------------------------------- load data 


output_summary_best <- read.csv(file.path("output", "output_exp_16", "run_summary_exp_16.csv"))


# ---------------------------------------- plot cost vs target level 


plot.file.name <- sprintf("Costs_vs_TargetLevel_exp_%s%s", parameters$Exp, ".tiff")

tiff(filename = file.path("figures", paste("figures_exp", parameters$Exp, sep = "_"), plot.file.name), 
     width = 8, height = 5, units = "in", compression = "lzw", res = 300)

ggplot(output_summary_best, aes(x = target_level, y = cost)) +
  geom_point(aes(x = target_level, y = cost), size = 1) +
  scale_x_continuous(expression(paste("Target level (km"^2, ")", sep="")),
                     limits = c(0, 10000),
                     breaks = seq(0, 10000, 1000)) +
  scale_y_continuous(expression(paste("Cost ($AU 10"^6, ")", sep="")),
                     limits=c(0, 18),
                     breaks=seq(0, 18, 2)) +
  theme(axis.text.x=element_text(angle = 0, vjust = 1, hjust = 0.5),
        axis.title.x=element_text(vjust = 0.5),
        axis.title.y=element_text(vjust = 0.5),
        plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm")) 

dev.off()
