average_responses <- function(responses_ind_experts, vars, base_info, fauna_ex_ind)
{

  species_responses <- NULL
  
  for (i in 1:length(fauna_ex_ind)) {
    
    out <- matrix(0, nrow = nrow(responses_ind_experts[[1]]), ncol = length(vars))
    
    colnames(out) <- 1:dim(out)[2]
    colnames(out) <- vars
    
    for (j in 1:length(vars)) {
      
      inds <- fauna_ex_ind[[i]]
      #cat("indices =", inds, "\n") #debugging
      
      variable <- vars[j]
      #cat("variable =", variable, "\n") #debugging
      
      all_experts_variable_values <- sapply(responses_ind_experts[inds], "[[", variable)
      
      out[, j] <- rowMeans(all_experts_variable_values)
      
    }    
    
    out <- cbind(responses_ind_experts[[inds[1]]][, base_info], out)
    
    species_responses <- rbind(species_responses, out)
    
  }
  
  species_responses  
}
