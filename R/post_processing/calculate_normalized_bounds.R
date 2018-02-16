cal_norm_lower_bound <- function(best_g, lower_b, confid, possib_lev){
  
  out <- best_g - (best_g - lower_b) * (possib_lev / confid)
  
}

cal_norm_upper_bound <- function(best_g, upper_b, confid, possib_lev){
  
  out <- best_g + (upper_b - best_g) * (possib_lev / confid)
  
}
