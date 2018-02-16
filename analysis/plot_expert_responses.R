# load R packages
library(ggplot2)
library(grid)
library(gridExtra)

# load R functions
source(file.path('R','post_processing','plotting_expert_responses.R'))

# load data
Species_Responses_Data <- read.csv (file.path('data', 'Species_Responses_all_threats.csv'), header=TRUE, sep = ',')

plotting.expert.responses (Species_Responses_Data)
