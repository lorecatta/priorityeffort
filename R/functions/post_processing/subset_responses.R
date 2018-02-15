subset_responses <- function(x, wanted_threats) {
  
  x <- x[x$Threat %in% wanted_threats, ]
  new_threat_numbering <- c(rep(1, 18), rep(2, 18), rep(3, 18), rep(4, 18)) 
  x$Threat <- new_threat_numbering
  x

}
