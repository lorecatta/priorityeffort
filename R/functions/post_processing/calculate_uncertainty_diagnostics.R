calc_uncertainty_diagnostics <- function(x){
  
  diagnostics <- c(
    "observed", 
    "expected", 
    "error", 
    "mean_perc_change", 
    "sd_mean_perc_change",
    "se_mean_perc_change",
    "prop_below",
    "prop_above",
    "prop_at",
    "Msf_neg",
    "SEsf_neg",
    "Msf_pos",
    "SEsf_pos")
  
  output_vec <- setNames(rep(0, length(diagnostics)), diagnostics)
  
  no_reps <- nrow(x)
    
  prop_below <- length(which(x$observed < x$target)) / no_reps
  prop_above <- length(which(x$observed >= x$target)) / no_reps
  prop_at <- length(which(x$observed == x$target)) / no_reps
  
  # sum errors across species  
  error_sums <- colSums(x[, c("observed", "expected", "error")])
  
  mean_perc_change <- mean(x$perc_change)
  sd_mean_perc_change <- sd(x$perc_change)
  se_mean_perc_change <- sd_mean_perc_change / sqrt(length(x$perc_change))
  
  neg_sf_ids <- which(x$shortfall < 0)
  pos_sf_ids <- which(x$shortfall > 0)
  
  mean_neg_sf <- mean(x$shortfall[neg_sf_ids])
  sd_m_neg_sf <- sd(x$shortfall[neg_sf_ids])
  se_m_neg_sf <- sd_m_neg_sf / sqrt(length(x$shortfall[neg_sf_ids]))
    
  mean_pos_sf <- mean(x$shortfall[pos_sf_ids])
  sd_m_pos_sf <- sd(x$shortfall[pos_sf_ids])
  se_m_pos_sf <- sd_m_pos_sf / sqrt(length(x$shortfall[pos_sf_ids]))
  
  output_vec[] <- c(error_sums[1], 
                    error_sums[2], 
                    error_sums[3], 
                    mean_perc_change, 
                    sd_mean_perc_change,
                    se_mean_perc_change,
                    prop_below,
                    prop_above,
                    prop_at,
                    mean_neg_sf,
                    se_m_neg_sf,
                    mean_pos_sf,
                    se_m_pos_sf)
  
  output_vec
}  
