create_exp_des <- function(parms) {

  experiment_ID <- parms$Exp
  no.replicates <- parms$Replicates
  target_level_val <- parms$TargetLevel

  fixed_targets <- parms$fixed_targets

  if(fixed_targets){

    out <- expand.grid(ID_exp = experiment_ID,
                       ID_run = seq_len(no.replicates),
                       target_level = target_level_val)

  } else {

    out <- expand.grid(ID_exp = experiment_ID,
                       ID_run = seq_len(no.replicates))

  }

  out$ID_run <- seq_len(nrow(out))

  out
}
