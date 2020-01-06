dbQ <- function(query, con) {

  res <- as.data.table(dbFetch(dbSendQuery(con, query),
                               n = -1))
  print("result from dbq")
  print(res)
  return(res)
}
