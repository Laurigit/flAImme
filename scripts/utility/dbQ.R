dbQ <- function(query, con) {
if (!is.null(con)) {
  res <- as.data.table(dbFetch(dbSendQuery(con, query),
                               n = -1))
  print("result from dbq")
  print(res)
} else {
  res <- "Error in dbQ, no con"
}
  return(res)
}
