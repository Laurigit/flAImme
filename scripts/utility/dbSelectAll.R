dbSelectAll <- function(table, con) {

  res <- as.data.table(dbFetch(dbSendQuery(con, paste0("SELECT * FROM ",
                                                    table)),
                               n = -1))
  return(res)
}


