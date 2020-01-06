op <- function(muuttuja) {
  paste0(muuttuja, ": ", get(muuttuja, envir = parent.frame()))
}
