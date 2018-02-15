# load R packages
library(ggplot2)
library(grid)
library(gridExtra)

# get the selected area for different target area and target persistence values 
selected_effort_1000_0.6 <- get.selected_effort (combined_output_summary_bests, combined_all_run_results, 1000, 0.6)
selected_effort_1000_0.7 <- get.selected_effort (combined_output_summary_bests, combined_all_run_results, 1000, 0.7)
selected_effort_1000_0.8 <- get.selected_effort (combined_output_summary_bests, combined_all_run_results, 1000, 0.8)
selected_effort_1000_0.9 <- get.selected_effort (combined_output_summary_bests, combined_all_run_results, 1000, 0.9)
selected_effort_1000_1 <- get.selected_effort (combined_output_summary_bests, combined_all_run_results, 1000, 1)

selected_effort_5000_0.6 <- get.selected_effort (combined_output_summary_bests, combined_all_run_results, 5000, 0.6)
selected_effort_5000_0.7 <- get.selected_effort (combined_output_summary_bests, combined_all_run_results, 5000, 0.7)
selected_effort_5000_0.8 <- get.selected_effort (combined_output_summary_bests, combined_all_run_results, 5000, 0.8)
selected_effort_5000_0.9 <- get.selected_effort (combined_output_summary_bests, combined_all_run_results, 5000, 0.9)
selected_effort_5000_1 <- get.selected_effort (combined_output_summary_bests, combined_all_run_results, 5000, 1)

selected_effort_10000_0.6 <- get.selected_effort (combined_output_summary_bests, combined_all_run_results, 10000, 0.6)
selected_effort_10000_0.7 <- get.selected_effort (combined_output_summary_bests, combined_all_run_results, 10000, 0.7)
selected_effort_10000_0.8 <- get.selected_effort (combined_output_summary_bests, combined_all_run_results, 10000, 0.8)
selected_effort_10000_0.9 <- get.selected_effort (combined_output_summary_bests, combined_all_run_results, 10000, 0.9)
selected_effort_10000_1 <- get.selected_effort (combined_output_summary_bests, combined_all_run_results, 10000, 1)


# construct the corresponding plots
plot_1 <- plot.selected.effort (selected_effort_1000_0.6, 1, 0.1, 0, 0.2, element_blank(), element_blank())
plot_2 <- plot.selected.effort (selected_effort_1000_0.7, 0, 0.1, 0, 0.2, element_blank(), element_blank())
plot_3 <- plot.selected.effort (selected_effort_1000_0.8, 0, 0.1, 0, 0.2, element_blank(), element_blank())
plot_4 <- plot.selected.effort (selected_effort_1000_0.9, 0, 0.1, 0, 0.2, element_blank(), element_blank())
plot_5 <- plot.selected.effort (selected_effort_1000_1, 0, 0.1, 0.5, 0.2, element_text(angle = 45, hjust = 1, vjust = 1), element_line())

plot_6 <- plot.selected.effort (selected_effort_5000_0.6, 1, 0.1, 0, 0.2,element_blank(), element_blank())
plot_7 <- plot.selected.effort (selected_effort_5000_0.7, 0, 0.1, 0, 0.2, element_blank(),element_blank())
plot_8 <- plot.selected.effort (selected_effort_5000_0.8, 0, 0.1, 0, 0.2, element_blank(), element_blank())
plot_9 <- plot.selected.effort (selected_effort_5000_0.9, 0, 0.1, 0, 0.2, element_blank(), element_blank())
plot_10 <- plot.selected.effort (selected_effort_5000_1, 0, 0.1, 0.5, 0.2, element_text(angle = 45, hjust = 1, vjust = 1), element_line())

plot_11 <- plot.selected.effort (selected_effort_10000_0.6, 1, 0.1, 0, 0.2, element_blank(), element_blank())
plot_12 <- plot.selected.effort (selected_effort_10000_0.7, 0, 0.1, 0, 0.2, element_blank(), element_blank())
plot_13 <- plot.selected.effort (selected_effort_10000_0.8, 0, 0.1, 0, 0.2, element_blank(), element_blank())
plot_14 <- plot.selected.effort (selected_effort_10000_0.9, 0, 0.1, 0, 0.2, element_blank(), element_blank())
plot_15 <- plot.selected.effort (selected_effort_10000_1, 0, 0.1, 0.5, 0.2, element_text(angle = 45, hjust = 1, vjust = 1), element_line())

# get the legend 
legend_1 <- g_legend(plot_1)

# save as tiff file 
tiff(filename = file.path("figures", "exp_10", "area_selected.tif"),
     width = 33, height = 23, units = "cm", pointsize = 12,
     compression = "lzw",
     bg = "white", res = 300)

grid.arrange(rbind(ggplotGrob(plot_1 + theme(legend.position = "none")), 
                   ggplotGrob(plot_2 + theme(legend.position = "none")), 
                   ggplotGrob(plot_3 + theme(legend.position = "none")), 
                   ggplotGrob(plot_4 + theme(legend.position = "none")), 
                   ggplotGrob(plot_5 + theme(legend.position = "none")), size="last"), 
             rbind(ggplotGrob(plot_6 + theme(legend.position = "none")), 
                   ggplotGrob(plot_7 + theme(legend.position = "none")), 
                   ggplotGrob(plot_8 + theme(legend.position = "none")), 
                   ggplotGrob(plot_9 + theme(legend.position = "none")), 
                   ggplotGrob(plot_10 + theme(legend.position = "none")), size="last"), 
             rbind(ggplotGrob(plot_11 + theme(legend.position = "none")), 
                   ggplotGrob(plot_12 + theme(legend.position = "none")), 
                   ggplotGrob(plot_13 + theme(legend.position = "none")), 
                   ggplotGrob(plot_14 + theme(legend.position = "none")), 
                   ggplotGrob(plot_15 + theme(legend.position = "none")), size="last"), legend_1, ncol = 4, 
             left=textGrob("Number of selected planning units", rot=90, gp=gpar(fontsize=18)),
             widths=c(0.88/3,0.88/3,0.88/3,0.12))

grid.text("Target area = 1000", x = unit(6.5, "cm"), y = unit(22.8, "cm"), gp = gpar(fontsize=12))
grid.text("Target area = 5000", x = unit(15.5, "cm"), y = unit(22.8, "cm"), gp = gpar(fontsize=12))
grid.text("Target area = 10000", x = unit(24.8, "cm"), y = unit(22.8, "cm"), gp = gpar(fontsize=12))

#grid.text("Pers = 0.6", x = unit(20.8, "cm"), y = unit(15.8, "cm"), rot=-90, gp = gpar(fontsize=12))
#grid.text("Pers = 0.7", x = unit(20.8, "cm"), y = unit(12.8, "cm"), rot=-90, gp = gpar(fontsize=12))
#grid.text("Pers = 0.8", x = unit(20.8, "cm"), y = unit(10, "cm"), rot=-90, gp = gpar(fontsize=12))
#grid.text("Pers = 0.9", x = unit(20.8, "cm"), y = unit(7.2, "cm"), rot=-90, gp = gpar(fontsize=12))
#grid.text("Pers = 1", x = unit(20.8, "cm"), y = unit(4.4, "cm"), rot=-90, gp = gpar(fontsize=12))

dev.off()
