attach_target_col <- function(i, run_dts, exp_tab) {
  id <- run_ids[i]
  dts <- run_dts[[i]] 
  cbind(dts, target = exp_tab[exp_tab$ID_run == id, "target_level"])
}
