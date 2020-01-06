save_db_table_to_RData <- function(con, table, folder) {
  res_table <-  dbSelectAll(table, con)
  assign(table, res_table)
  save(list = table, file = paste0(folder, table, ".RData") )
}
