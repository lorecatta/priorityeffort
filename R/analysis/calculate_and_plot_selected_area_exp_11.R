# load R packages
library(ggplot2)
library(grid)
library(gridExtra)

# load R functions
source(file.path("R","post_processing","calculate_area_selected_with_management_effort.R"))

# get the selected area for different target levels 
selected_effort_1 <- get.selected_effort (output_summary_exp_11_best, results_exp_11, 400)
selected_effort_2 <- get.selected_effort (output_summary_exp_11_best, results_exp_11, 1000)
selected_effort_3 <- get.selected_effort (output_summary_exp_11_best, results_exp_11, 6000)

# construct the corresponding plots
plot_1 <- plot.selected.effort (selected_effort_1, 1, 0.1, 0, 0.2, element_blank(), element_blank())
plot_2 <- plot.selected.effort (selected_effort_2, 0, 0.1, 0, 0.2, element_blank(), element_blank())
plot_3 <- plot.selected.effort (selected_effort_3, 0, 0.1, 0.5, 0.2, element_text(angle = 45, hjust = 1, vjust = 1), element_line())


#plot_4 <- plot.selected.effort (selected_effort_1000_0.9, 0, 0.1, 0, 0.2, element_blank(), element_blank())
#plot_5 <- plot.selected.effort (selected_effort_1000_1, 0, 0.1, 0.5, 0.2, element_text(angle = 45, hjust = 1, vjust = 1), element_line())

#plot_6 <- plot.selected.effort (selected_effort_5000_0.6, 1, 0.1, 0, 0.2,element_blank(), element_blank())
#plot_7 <- plot.selected.effort (selected_effort_5000_0.7, 0, 0.1, 0, 0.2, element_blank(),element_blank())
#plot_8 <- plot.selected.effort (selected_effort_5000_0.8, 0, 0.1, 0, 0.2, element_blank(), element_blank())
#plot_9 <- plot.selected.effort (selected_effort_5000_0.9, 0, 0.1, 0, 0.2, element_blank(), element_blank())
#plot_10 <- plot.selected.effort (selected_effort_5000_1, 0, 0.1, 0.5, 0.2, element_text(angle = 45, hjust = 1, vjust = 1), element_line())

#plot_11 <- plot.selected.effort (selected_effort_10000_0.6, 1, 0.1, 0, 0.2, element_blank(), element_blank())
#plot_12 <- plot.selected.effort (selected_effort_10000_0.7, 0, 0.1, 0, 0.2, element_blank(), element_blank())
#plot_13 <- plot.selected.effort (selected_effort_10000_0.8, 0, 0.1, 0, 0.2, element_blank(), element_blank())
#plot_14 <- plot.selected.effort (selected_effort_10000_0.9, 0, 0.1, 0, 0.2, element_blank(), element_blank())
#plot_15 <- plot.selected.effort (selected_effort_10000_1, 0, 0.1, 0.5, 0.2, element_text(angle = 45, hjust = 1, vjust = 1), element_line())

# get the legend 
legend_1 <- g_legend(plot_1)

# save as tiff file 
tiff(filename = file.path("figures", paste("figures_exp", parameters$Exp, sep="_"), "area_selected.tiff"),
     width = 15, height = 15, units = "cm", pointsize = 12,
     compression = "lzw",
     bg = "white", res = 300)

grid.arrange(rbind(ggplotGrob(plot_1 + theme(legend.position = "none")), 
                   ggplotGrob(plot_2 + theme(legend.position = "none")), 
                   ggplotGrob(plot_3 + theme(legend.position = "none")), size="last"), legend_1, ncol = 2, 
             left=textGrob("Number of selected planning units", rot=90, gp=gpar(fontsize=16)),
             widths=c(2/3, 1/3))

#grid.text("Target area = 1000", x = unit(6.5, "cm"), y = unit(22.8, "cm"), gp = gpar(fontsize=12))
#grid.text("Target area = 5000", x = unit(15.5, "cm"), y = unit(22.8, "cm"), gp = gpar(fontsize=12))
#grid.text("Target area = 10000", x = unit(24.8, "cm"), y = unit(22.8, "cm"), gp = gpar(fontsize=12))

#grid.text("Pers = 0.6", x = unit(20.8, "cm"), y = unit(15.8, "cm"), rot=-90, gp = gpar(fontsize=12))
#grid.text("Pers = 0.7", x = unit(20.8, "cm"), y = unit(12.8, "cm"), rot=-90, gp = gpar(fontsize=12))
#grid.text("Pers = 0.8", x = unit(20.8, "cm"), y = unit(10, "cm"), rot=-90, gp = gpar(fontsize=12))
#grid.text("Pers = 0.9", x = unit(20.8, "cm"), y = unit(7.2, "cm"), rot=-90, gp = gpar(fontsize=12))
#grid.text("Pers = 1", x = unit(20.8, "cm"), y = unit(4.4, "cm"), rot=-90, gp = gpar(fontsize=12))

dev.off()
