plot_trackers <- function(solution){

  ann_summary <- solution$summary
  ID_exp <- solution$exp
  ID_run <- solution$run

  ann_summary_df <- as.data.frame(ann_summary)

  no_iterations <- max(ann_summary_df$Iter)
  #cat("number of iterations = ", no_iterations, "\n") #debugging
  max_cost_value <- max(ann_summary_df$CostCount)
  #cat("max cost value = ", max_cost_value, "\n") #debugging
  max_penalty_value <- max(ann_summary_df$PenaltyCount)
  #cat("max penalty value = ", max_penalty_value, "\n") #debugging

  #####
  temperature_plot <- ggplot(ann_summary_df, aes(x=Iter, y=Temp)) +
    ggtitle("Annealing temperature") +
    geom_line(aes(x=Iter, y=Temp), size=0.3) +
    scale_x_continuous("Iteration",
                       limits=c(1, no_iterations),
                       breaks=seq(1, no_iterations, length.out = 5),
                       labels=floor(seq(1, no_iterations, length.out = 5))) +
    scale_y_continuous("Temperature",
                       limits=c(0,1),
                       breaks=seq(0,1,0.2)) +
    theme(axis.title.x = element_text(hjust=0.5, vjust=1),
          axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
          axis.title.y = element_text(hjust=0.5, vjust=0),
          axis.text=element_text(size=12),
          plot.margin=unit(c(1,1,1,1), "cm"),
          plot.title = element_text(lineheight=1, face="bold"))

  cost_plot <- ggplot(ann_summary_df, aes(x=Iter, y=CostCount)) +
    ggtitle("Cost") +
    geom_line(aes(x=Iter, y=CostCount), size=0.3, colour="blue") +
    scale_x_continuous("Iteration",
                       limits=c(1, no_iterations),
                       breaks=seq(1, no_iterations, length.out = 5),
                       labels=floor(seq(1, no_iterations, length.out = 5))) +
    scale_y_continuous("Cost",
                       limits=c(0,max_cost_value),
                       breaks=floor(seq(0,max_cost_value,length.out = 6))) +
    theme(axis.title.x = element_text(hjust=0.5, vjust=1),
          axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
          axis.title.y = element_text(hjust=0.5, vjust=0),
          axis.text=element_text(size=12),
          plot.margin=unit(c(1,1,1,1), "cm"),
          plot.title = element_text(lineheight=1, face="bold"))

  penalty_plot <- ggplot(ann_summary_df, aes(x=Iter, y=PenaltyCount)) +
    ggtitle("Species penalty") +
    geom_line(aes(x=Iter, y=PenaltyCount), size=0.3, colour="red") +
    scale_x_continuous("Iteration",
                       limits=c(1, no_iterations),
                       breaks=seq(1, no_iterations, length.out = 5),
                       labels=floor(seq(1, no_iterations, length.out = 5))) +
    scale_y_continuous("Species penalty",
                       limits=c(0,max_penalty_value),
                       breaks=floor(seq(0,max_penalty_value,length.out = 6))) +
    theme(axis.title.x = element_text(hjust=0.5, vjust=1),
          axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
          axis.title.y = element_text(hjust=0.5, vjust=0),
          axis.text=element_text(size=12),
          plot.margin=unit(c(1,1,1,1), "cm"),
          plot.title = element_text(lineheight=1, face="bold"))

  # lines(1:nrow(ann_summary_df), pen, col="red")
  # lines(1:nrow(ann_summary_df), cost, col="blue")
  # legend("topright", c("Penalty","Cost"), lty=c(1,1), col=c("red","blue"), bty="n",cex=1.2)

  out_path <- file.path("figures", paste("figures_exp", ID_exp, sep="_"), "trackers")
  dir.create(out_path, FALSE, TRUE)
  plot_name <- sprintf("tracker_%s%s", paste("run", ID_run, sep="_"), ".png")

  png(file.path(out_path, plot_name),
      width = 9,
      height = 10,
      units = "in",
      res = 300)

  grid.arrange(temperature_plot,
               cost_plot,
               penalty_plot,
               ncol = 1,
               top = textGrob(paste('Run', ID_run, sep = " "), rot = 0, gp = gpar(fontsize = 18)))

  dev.off()

}
