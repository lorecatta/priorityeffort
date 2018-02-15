RDI_Daly_2_diss <- read.table('C:/Users/s2872641/Documents/Griffith_post_doc/Data_Daly/Threats_Daly/RDI_Daly_2_0.5.txt', header=TRUE, sep = ",")
RDI_Daly_2_diss <- RDI_Daly_2_diss [,c('FID_','FID_Catch_','Shape_Area','UPSDIST','FRDI')]
RDI_Daly_2_diss <- as.matrix(RDI_Daly_2_diss)
RDI_Daly_2_diss <- cbind(RDI_Daly_2_diss, target=rep(0,nrow(RDI_Daly_2_diss)))
RDI_Daly_2_diss [,1] <- 0:(nrow(RDI_Daly_2_diss )-1)

for (i in 0:864)
{

  if(any(unique(RDI_Daly_2_diss [,2])==i))
  {
    
    Subset <- RDI_Daly_2_diss[RDI_Daly_2_diss[,'FID_Catch_']==i,] 
    Subset <- matrix(Subset,ncol=6)
  
    if(nrow(Subset)>1)
    {
      index <- which(Subset[,4]==max(Subset[,4]))[1]
      ID_pol <- Subset[index,1] 
    }else{
      ID_pol <- Subset[1]
    }
  
    #show(i)
    #show(ID_pol)
    RDI_Daly_2_diss[RDI_Daly_2_diss [,1]==ID_pol,6] <- 1
  
  }else{
    
  }
}

RDI_Daly_2_diss_PUs <- RDI_Daly_2_diss[which(RDI_Daly_2_diss[,6]==1),]

add <- cbind(c(19701,19702,19703,19074,19075,19076,19077,19078,19079,19080,19081,19082,19083,19084,19085,19086,19087,19088,19089,19090,19091,19092,19093), c(12,134,176,347,478,508,569,650,706,708,709,715,718,742,743,746,758,759,760,762,765,766,768), rep(0,23), rep(0,23), c(0.10104,0,0,0,0.467984,0.445507,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), rep(1,23))

RDI_Daly_2_diss_PUs <- rbind(RDI_Daly_2_diss_PUs, add)

#also add
RDI_Daly_2_diss_PUs [RDI_Daly_2_diss_PUs[,2]==770,5] <- 0.04

#check 
length(unique(RDI_Daly_2_diss_PUs [,2]))

write.table(RDI_Daly_2_diss_PUs,'C:/Users/s2872641/Documents/Griffith_post_doc/Data_Daly/Threats_Daly/RDI_Daly_2_diss_PUs_0.5.txt',sep = ",", col.names = NA)



#################check missing values in a sequence 
for (i in 0:864)
{
  if(any(unique(RDI_Daly_2_diss [,2])==i))
  {
    print('good') 
  }else{
    cat(paste('questo numero manca',i,sep='='), any(unique(RDI_Daly_2_diss [,2])==i), '\n')
  }
}


