###
# function for calculating the number of planning units with selected levels of effort for different actions 
###
get.selected_effort <- function(output_summary, all_run_results, target_level)
{
  no.actions <- 4
  no.levels <- 4
  no.estimates <- 3

  out_df <- NULL
  
  for (j in 1:no.estimates)
  {
    id_run <- output_summary[which(output_summary[,"estimate"]==j & output_summary[,"target_level"]==target_level), "ID_run"]
    
    cat("id run = ", id_run, "\n") #debugging
  
    # get site action array from the best run for a particular estimate, target persistence and target area combination
    site_action_array <- all_run_results[[id_run]][7][[1]]
  
    out <- matrix(0, nrow=no.levels, ncol=no.actions)
    
    for (i in 1:no.levels)
    {
      out [i,] <- apply(site_action_array, 2, function(x) {length(which(x==i))})
    }
  
    out_df_one_estimate <- cbind(expand.grid(estimate=j, level_of_effort=1:no.levels, action=1:no.actions), no.pu=as.vector(out))
    out_df <- rbind(out_df, out_df_one_estimate) 
  }

  
  out_df$level_of_effort <- as.factor(out_df$level_of_effort)
  out_df$action <- factor(out_df$action, levels=c(1,2,3,4), labels=c("Buffalo shooting", "Pig shooting", "Riparian restoration", "Chemical spraying"))
  out_df$estimate <- factor(out_df$estimate, levels=c(1,2,3), labels=c("Best guess", "Lower bound", "Upper bound"))
  
  out_df
}

###
# function for creating bar plot of number of selected planning units with different levels of effort (stacked) for different actions 
###
plot.selected.effort <- function(selected_effort, top_margin, right_margin, bottom_margin, left_margin, axis_text_x, axis_ticks_x)
{  
  roundUpNice <- function(x, nice=seq(1,10,0.5)) 
  {
    if(length(x) != 1) stop("x must be of length 1")
    10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
  }
  
  agg_effort <- aggregate(no.pu ~ estimate + action, data = selected_effort, sum)
  
  max_num_planning_units <- max(agg_effort$no.pu) 
  max_num_planning_units_round <- roundUpNice(max_num_planning_units)
  
  ggplot(selected_effort, aes(x=action, y=no.pu, fill=level_of_effort)) + 
  facet_grid(~estimate) + 
  geom_bar(stat="identity",
           colour="white",
           size=.3) +      # Thinner lines
  scale_y_continuous(limits=c(0, max_num_planning_units_round)) +
  theme_bw() +
  theme(axis.text.x=axis_text_x,
        axis.title.x=element_blank(),
        axis.ticks.x=axis_ticks_x,
        axis.title.y=element_blank(),
        plot.margin=unit(c(top_margin,right_margin,bottom_margin,left_margin), "cm")) +
  scale_fill_brewer(name="Level of\nmanagement effort", # Legend label, use darker colors
                    breaks=c("1", "2", "3", "4"),
                    labels=c("Baseline", "Low", "Medium", "High"),
                    palette="Spectral")
}

###
# function for separating legend from plot
###
g_legend<-function(a.gplot)
{
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}  
