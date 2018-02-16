create_exp_des <- function (parms, Offset) {

  experiment_ID <- parms$Exp
  no.replicates <- parms$Replicates

  CP_val <- parms$CPValues
  estimate_val <- parms$Estimate
  target_level_val <- parms$TargetLevel

  values <- expand.grid(ID_exp = experiment_ID,
                        ID_run = seq_len(no.replicates),
                        CP = CP_val,
                        estimate = estimate_val,
                        target_level = target_level_val)

  values$ID_run <- seq(from=1+Offset, to=nrow(values)+Offset, by=1)

  values

}
