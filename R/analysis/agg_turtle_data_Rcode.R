Species_Daly_final_inters <- read.table('C:/Users/s2872641/Documents/Griffith_post_doc/Data_Daly/Species_Daly/Turtles_Daly_final_inters.txt', header=TRUE, sep = ",")
Species_Daly_final_inters <- Species_Daly_final_inters[,c(2,5,60:67)]
Species_Daly_final_inters <- as.matrix(Species_Daly_final_inters)

#select a size threshold (sq km) to remove smaller areas
larger_areas_indices <- which(Species_Daly_final_inters [,2]>=0.5)
smaller_areas_indices <- which(Species_Daly_final_inters [,2]<0.5)

larger_areas <- Species_Daly_final_inters [larger_areas_indices,]
smaller_areas <- Species_Daly_final_inters [smaller_areas_indices,]

nspecies <- 8

#create a vector of the PU for which there is no available species distribution info
no_data <- c(706,709,718,742,758,759,760,762,766)

#dataset <- larger_areas
dataset <- smaller_areas

mat <- matrix(0, ncol=9, nrow=865)
colnames(mat) <- 1:dim(mat)[2]
col_names <- colnames(dataset)
col_names[2] <- 'PU_ID'
col_names <- col_names[2:10]
colnames(mat) <- col_names
  
for (i in 0:864)
{

  if(any(unique(dataset[,'FID_Catch_'])==i))
  {
    
    mat[i+1,1] <- i
    Subset <- matrix(dataset[dataset[,'FID_Catch_']==i,], ncol=ncol(dataset))
      
    for (j in 1:nspecies)
    {

      if(nrow(Subset)==1)
      {
      
        Area <- as.numeric(dataset[which(dataset[,1]==i),'Shape_Area'])
        
        if(Subset[,3:ncol(Subset)][j]==1)
        {
    
          mat[i+1,2:ncol(mat)][j] <- Area
        
        }

      }else{

        indices <- which(Subset[,3:ncol(Subset)][,j]==1)  
        Area <- sum (Subset[indices,2])
        
        mat[i+1,j+1] <- Area
        
      }
        
    }  
    
  }else{
  
    if(any(no_data==i))
    
    {
      #distinguish no data values
      mat[i+1,1] <- 1000
    
    }
  }
  
}

#agg_larger_areas <- mat 
agg_smaller_areas <- mat

#now pick the planning units that you have lost when you eliminated the small areas

for (i in 2:865)
{
  
  if(agg_larger_areas[i,'PU_ID']==0)
  {
  
    agg_larger_areas[i,] <- agg_smaller_areas[i,]
    
  }
  
}

#check
#which(agg_larger_areas[,'PU_ID']==0)

agg_larger_areas[which(agg_larger_areas[,'PU_ID']==1000),'PU_ID'] <- no_data

write.table(agg_larger_areas,'C:/Users/s2872641/Documents/Griffith_post_doc/Data_Daly/Species_Daly/agg_turtle_data.txt',sep = ",", col.names = NA)
  