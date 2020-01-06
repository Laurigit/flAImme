dbIns <- function(table, data_rows, con) {
  # data_rows <- data.table(PELI_ID = 1:10, TI_NAME = "A", T2_NAME= "B", CLOSING = as.character(now()))
  colnames <- paste0(colnames(data_rows), collapse = ", ")
  data_to_string <- apply(data_rows, 1, paste, collapse = '\',\'')

  for (loop in 1:nrow(data_rows)) {
    row_string <- data_to_string[loop]

    dbFetch(dbSendQuery(con, paste0('insert into ',
                                    table,
                                    ' (',
                                    colnames,
                                    ")",
                                    ' VALUES (\'',
                                    row_string,
                                    '\')')))

  }

}
