

getMode <- function(v) {
  dt <- data.table(arvo = v)
  dt[, maara := .N, by = arvo]
  res <- dt[which.max(maara), arvo]
  return(res)
}
