buffalo_incidence <- read.table(file.path("data", "threats", "buffalo_incidence.txt"), header = TRUE, sep = ",")
buffalo_incidence <- cbind(buffalo_incidence, buff_density = rep(0, nrow(buffalo_incidence)))
for (i in 1:nrow(buffalo_incidence))
{
  
  incidence_class <- buffalo_incidence[i,"buff_inc"]
  
  if(incidence_class == 1)
  {
    
    buffalo_incidence [i,"buff_density"] <- 0.05
    
  }
  
  if(incidence_class==2)
  {
    
    buffalo_incidence [i,"buff_density"] <- 3
    
  }
  
}
pig_incidence <- read.table(file.path("data", "threats", "pig_incidence.txt"), header = TRUE, sep = ",")
pig_incidence <- cbind(pig_incidence, pig_density = rep(0, nrow(pig_incidence)))
for (i in 1:nrow(pig_incidence))
{
  
  incidence_class <- pig_incidence[i,"pig_inc"]
  
  if(incidence_class == 1)
  {
    
    pig_incidence [i,"pig_density"] <- 3
    
  }
  
  if(incidence_class == 2)
  {
    
    pig_incidence [i,"pig_density"] <- 7.5
    
  }
  
}

daly_planning_unit_area <- read.table (file.path("data", "planning_unit_area_ha.txt"), header = TRUE, sep = ",")

effort_hours <- data.frame(pu_id = seq_len(nrow(daly_planning_unit_area)), 
                           area_km2 = daly_planning_unit_area$Shape_Area * 0.01,
                           buffalo_density = buffalo_incidence$buff_density, 
                           pig_density = pig_incidence$pig_density)

effort_hours$buffalo_hr_ind <- 0.102 * (effort_hours$buffalo_density^-0.673)
effort_hours$pig_hr_ind <- 0.1464 * (effort_hours$pig_density^-1.445)
effort_hours$tot_buffalos <- effort_hours$area_km2 * effort_hours$buffalo_density 
effort_hours$tot_pigs <- effort_hours$area_km2 * effort_hours$pig_density
effort_hours$tot_buffalo_hrs <- effort_hours$buffalo_hr_ind * effort_hours$tot_buffalos
effort_hours$tot_pig_hrs <- effort_hours$pig_hr_ind * effort_hours$tot_pigs

effort_hours[is.na(effort_hours)] <- 0

write.table(effort_hours, 
            file.path("data", "threats", "effort_hour_calculation.csv"), row.names = FALSE, sep = ",")
