#' Loop using parLapply.
#' @export
loop <- function(..., parallel) {
  if (parallel) {
    parallel::parLapply(NULL, ...)
  } else {
    lapply(...)
  }
}

#' wrapper over loop function.
#' @export
loop_simplify <- function(..., what) {
  vapply(loop(...), identity, what)
}

write_out_rds <- function(dat, my_path, file_name) {

  dir.create(my_path, FALSE, TRUE)

  saveRDS(dat, file.path(my_path, file_name))

}

#' Save a csv file.
#' @export
write_out_csv <- function(dat, my_path, file_name) {

  dir.create(my_path, FALSE, TRUE)

  write.csv(
    dat,
    file.path(my_path, file_name),
    row.names = FALSE)

}

#' Save a text file.
#' @export
write_out_txt <- function(dat, my_path, file_name) {

  dir.create(my_path, FALSE, TRUE)

  write.table(
    dat,
    file.path(my_path, file_name),
    row.names = FALSE,
    sep = ",")

}

#' Convert data frame to list.
#' @export
df_to_list <- function (x, use_names) {
  keep <- c("names", "class", "row.names")
  at <- attributes(x)
  attributes(x) <- at[intersect(names(at), keep)]
  ret <- unname(lapply(split(x, seq_len(nrow(x))), as.list))
  if (!use_names) {
    ret <- lapply(ret, unname)
  }
  if (is.character(at$row.names)) {
    names(ret) <- at$row.names
  }
  ret
}
