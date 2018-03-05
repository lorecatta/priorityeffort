#' Create a set of parameter combinations to run the prioritization on.
#'
#' @param parms a list of parameters.
#'
#' @export
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

  out_path <- file.path("output", paste("exp", experiment_ID, sep = "_"))
  dir.create(out_path, FALSE, TRUE)
  out_file_name <- paste0("experimental_design", ".csv")
  write.csv(out, file.path(out_path, out_file_name), row.names = FALSE)
  out

}
