###
# Here I assume that different incidence clasess correspond to 
# different proportions of a planning unit covered with para grass 
###

# Incidence classes: 0, 1, 2, 4
# Corresponding proportions of para grass cover: 0, <10%, 10-50%, >50%
# For each proportion, I consider the mean of the range

# Load data
weed_incidence <- read.table(file.path("data", "threats", "weed_incidence.txt"), header = TRUE, sep = ",") 

weed_incidence <- cbind(weed_incidence, prop_para_grass = rep(0, nrow(weed_incidence)))

for (i in 1:nrow(weed_incidence))
{
  
  incidence_class <- weed_incidence[i, "MAX_paragr"]
  
  if(incidence_class == 1)
  {
    
    weed_incidence [i, "prop_para_grass"] <- 0.05
    
  }
  
  if(incidence_class == 2)
  {
    
    weed_incidence [i, "prop_para_grass"] <- 0.3
    
  }
  
}

write.table(weed_incidence, 
            file.path("data", "para_grass_prop_cover.txt"), row.names = FALSE, sep = ",")
