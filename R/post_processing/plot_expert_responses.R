plotting.expert.responses <- function(species_response_data)
{
  # keep only data for threats 1, 2, 4 ans 7
  species_responses <- species_response_data[which(species_response_data[,'Threat']==1 | species_response_data[,'Threat']==2 | species_response_data[,'Threat']==4 | species_response_data[,'Threat']==7),] 

  # change threat names
  species_responses[which(species_responses[,'Threat']==1),'Threat'] <- 'Buffalos'
  species_responses[which(species_responses[,'Threat']==2),'Threat'] <- 'Pigs'
  species_responses[which(species_responses[,'Threat']==4),'Threat'] <- 'Grazing'
  species_responses[which(species_responses[,'Threat']==7),'Threat'] <- 'Paragrass'
 
  # change faunal group names
  species_responses[which(species_responses[,'FaunalGroup']==1),'FaunalGroup'] <- 'Fishes'
  species_responses[which(species_responses[,'FaunalGroup']==2),'FaunalGroup'] <- 'Turtles'
  species_responses[which(species_responses[,'FaunalGroup']==3),'FaunalGroup'] <- 'Waterbirds'

  ## change ecological group names
  # for fishes 
  species_responses[which(species_responses[,'FaunalGroup']=='Fishes' & species_responses[,'EcologicalGroup']==1),'EcologicalGroup'] <- 'Large-bodied migratory carnivores'
  species_responses[which(species_responses[,'FaunalGroup']=='Fishes' & species_responses[,'EcologicalGroup']==2),'EcologicalGroup'] <- 'Large-bodied herbivore/omnivores'
  species_responses[which(species_responses[,'FaunalGroup']=='Fishes' & species_responses[,'EcologicalGroup']==3),'EcologicalGroup'] <- 'Large-bodied carnivores'
  species_responses[which(species_responses[,'FaunalGroup']=='Fishes' & species_responses[,'EcologicalGroup']==4),'EcologicalGroup'] <- 'Small-bodied migratory invertivore'
  species_responses[which(species_responses[,'FaunalGroup']=='Fishes' & species_responses[,'EcologicalGroup']==5),'EcologicalGroup'] <- 'Grunters'
  species_responses[which(species_responses[,'FaunalGroup']=='Fishes' & species_responses[,'EcologicalGroup']==6),'EcologicalGroup'] <- 'Small-bodied invertivore'
  # for turltes 
  species_responses[which(species_responses[,'FaunalGroup']=='Turtles' & species_responses[,'EcologicalGroup']==1),'EcologicalGroup'] <- 'Pig-nosed turtle'
  species_responses[which(species_responses[,'FaunalGroup']=='Turtles' & species_responses[,'EcologicalGroup']==2),'EcologicalGroup'] <- 'Sandstone snake-necked turtle'
  species_responses[which(species_responses[,'FaunalGroup']=='Turtles' & species_responses[,'EcologicalGroup']==3),'EcologicalGroup'] <- 'Northern snake-necked turtle'
  species_responses[which(species_responses[,'FaunalGroup']=='Turtles' & species_responses[,'EcologicalGroup']==4),'EcologicalGroup'] <- 'Northern snapping turtle'
  species_responses[which(species_responses[,'FaunalGroup']=='Turtles' & species_responses[,'EcologicalGroup']==5),'EcologicalGroup'] <- 'Common sawshell turtle'
  species_responses[which(species_responses[,'FaunalGroup']=='Turtles' & species_responses[,'EcologicalGroup']==6),'EcologicalGroup'] <- 'Short-necked turtles'
  # for waterbirds
  species_responses[which(species_responses[,'FaunalGroup']=='Waterbirds' & species_responses[,'EcologicalGroup']==1),'EcologicalGroup'] <- 'Ducks, small grebes and Jacana'
  species_responses[which(species_responses[,'FaunalGroup']=='Waterbirds' & species_responses[,'EcologicalGroup']==2),'EcologicalGroup'] <- 'Herbivores'
  species_responses[which(species_responses[,'FaunalGroup']=='Waterbirds' & species_responses[,'EcologicalGroup']==3),'EcologicalGroup'] <- 'Large wading birds'
  species_responses[which(species_responses[,'FaunalGroup']=='Waterbirds' & species_responses[,'EcologicalGroup']==4),'EcologicalGroup'] <- 'Small wading birds and shorebirds'
  species_responses[which(species_responses[,'FaunalGroup']=='Waterbirds' & species_responses[,'EcologicalGroup']==5),'EcologicalGroup'] <- 'Small piscivores'
  species_responses[which(species_responses[,'FaunalGroup']=='Waterbirds' & species_responses[,'EcologicalGroup']==6),'EcologicalGroup'] <- 'Large piscivores'

  # set factors for ggplot 
  species_responses$Intensity <- factor(species_responses$Intensity)
  species_responses$EcologicalGroup <- factor(species_responses$EcologicalGroup)
  species_responses$FaunalGroup <- factor(species_responses$FaunalGroup)
  species_responses$Threat <- factor(species_responses$Threat)

  # subdivide data in three faunal groups 
  species_responses_fishes <- species_responses[which(species_responses[,'FaunalGroup']=='Fishes'),] 
  species_responses_turtles <- species_responses[which(species_responses[,'FaunalGroup']=='Turtles'),]
  species_responses_waterbirds <- species_responses[which(species_responses[,'FaunalGroup']=='Waterbirds'),]
    
  #dev.new(width=20, height=15)

  plot_1 <- ggplot(species_responses_fishes, aes(x=Intensity, y=PP_BestGuess, fill=EcologicalGroup)) + 
    facet_grid(Threat~FaunalGroup) + 
    geom_bar(position=position_dodge(), stat="identity",
             colour="black", # Use black outlines,
             size=.3) +      # Thinner lines
    geom_errorbar(aes(ymin=norm_Lb, ymax=norm_Ub),
                  size=.5,    # Thinner lines
                  width=.5,
                  position=position_dodge(.9)) +
    xlab("") +
    ylab("Probability of persistence") +
    ggtitle("") +
    scale_y_continuous(limits=c(0, 1)) +
    theme_bw() +
    theme(plot.margin=unit(c(0.5,0,0.5,0.5), "cm")) +
    scale_fill_brewer(name="Ecological groups of fishes", # Legend label, use darker colors
                   breaks=c("Grunters", "Large-bodied carnivores", "Large-bodied herbivore/omnivores", "Large-bodied migratory carnivores", "Small-bodied invertivore", "Small-bodied migratory invertivore"),
                   labels=c("Grunters", "Large-bodied carnivores", "Large-bodied herbivore/omnivores", "Large-bodied migratory carnivores", "Small-bodied invertivore", "Small-bodied migratory invertivore"),
                   palette='Blues')
                   
  plot_2 <- ggplot(species_responses_turtles, aes(x=Intensity, y=PP_BestGuess, fill=EcologicalGroup)) + 
    facet_grid(Threat~FaunalGroup) + 
    geom_bar(position=position_dodge(), stat="identity",
             colour="black", # Use black outlines,
             size=.3) +      # Thinner lines
    geom_errorbar(aes(ymin=norm_Lb, ymax=norm_Ub),
                  size=.5,    # Thinner lines
                  width=.5,
                  position=position_dodge(.9)) +
    xlab("Threat intensity") +
        ggtitle("") +
    scale_y_continuous(limits=c(0, 1)) +
    theme_bw() +
    theme(axis.line=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.y=element_blank()) +
    theme(plot.margin=unit(c(0.5,0,0.5,0), "cm")) +
    scale_fill_brewer(name="Ecological groups of turtles", # Legend label, use darker colors
                   breaks=c("Common sawshell turtle", "Northern snake-necked turtle", "Northern snapping turtle", "Pig-nosed turtle", "Sandstone snake-necked turtle", "Short-necked turtles"),
                   labels=c("Common sawshell turtle", "Northern snake-necked turtle", "Northern snapping turtle", "Pig-nosed turtle", "Sandstone snake-necked turtle", "Short-necked turtles"),
                   palette='YlOrBr')

  plot_3 <- ggplot(species_responses_waterbirds, aes(x=Intensity, y=PP_BestGuess, fill=EcologicalGroup)) + 
    facet_grid(Threat~FaunalGroup) + 
    geom_bar(position=position_dodge(), stat="identity",
             colour="black", # Use black outlines,
             size=.3) +      # Thinner lines
    geom_errorbar(aes(ymin=norm_Lb, ymax=norm_Ub),
                  size=.5,    # Thinner lines
                  width=.5,
                  position=position_dodge(.9)) +
    xlab("") +
    ylab("Probability of persistence") +
    ggtitle("") +
    scale_y_continuous(limits=c(0, 1)) +
    theme_bw() +
    theme(axis.line=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.y=element_blank()) +
    theme(plot.margin=unit(c(0.5,0.5,0.5,0), "cm")) +
    scale_fill_brewer(name="Ecological groups of waterbirds", # Legend label, use darker colors
                   breaks=c("Ducks, small grebes and Jacana", "Herbivores", "Large piscivores", "Large wading birds", "Small piscivores", "Small wading birds and shorebirds"),
                   labels=c("Ducks, small grebes and Jacana", "Herbivores", "Large piscivores", "Large wading birds", "Small piscivores", "Small wading birds and shorebirds"),
                   palette='YlGn')

  g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    legend
  }

  legend_1 <- g_legend(plot_1)
  legend_2 <- g_legend(plot_2)
  legend_3 <- g_legend(plot_3)

  tiff(filename = file.path('figures', 'plot_of_expert_responses.tif'),
     width = 35, height = 20, units = "cm", pointsize = 12,
     compression = "lzw",
     bg = "white", res = 300)

  grid.arrange(cbind(ggplotGrob(plot_1 + theme(legend.position = 'none')), ggplotGrob(plot_2 + theme(legend.position = 'none')), ggplotGrob(plot_3 + theme(legend.position = 'none')), size="last"), rbind(legend_1,legend_2,legend_3, size="last"), ncol = 2, widths=c(4/5,1/5))

  dev.off()
}
