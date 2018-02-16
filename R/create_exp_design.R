create_exp_des <- function(parms) {

  experiment_ID <- parms$Exp
  no.replicates <- parms$Replicates

  estimate_val <- parms$Estimate
  target_level_val <- parms$TargetLevel

  expand.grid(ID_exp = experiment_ID,
              ID_run = seq_len(no.replicates),
              CP = CP_val,
              estimate = estimate_val,
              target_level = target_level_val)

}
