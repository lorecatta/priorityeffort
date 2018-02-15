combine_faunal_g_expert <- function(x, y){
  a <- x$f
  b <- x$t
  c <- x$b
  xx <- rbind(y[[a]], y[[b]], y[[c]])
}
