#force_key_cols on vektori valituista sarakkeista
dbIoU <- function(table, data_rows, con, force_key_cols = NULL) {

if (is.null(force_key_cols)) {
  keyCols <- suppressWarnings( dbQ(paste0("SHOW KEYS FROM ",table, " WHERE Key_name = 'PRIMARY'"), con)[, Column_name])


} else {
  keyCols <- force_key_cols
}
  #data_rows <- data.table(PELI_ID = 1:6, BETTER_ID = 1:4, ODDS = as.integer(runif(n = 24, min = 0, max = 100)), BET_DT = now() )

  #fix quotes to match sql
  cols <- colnames(data_rows)
  data_rows[ , (cols) := lapply(.SD, function(x_input){gsub('\'', '\'\'', x_input)}), .SDcols = cols]

  colnames_vect <- colnames(data_rows)
  colnames <- paste0(colnames_vect, collapse = ", ")
  non_key_cols <- setdiff(colnames_vect, keyCols)
  data_to_string <- apply(data_rows, 1, paste, collapse="\',\'")



  for (loop in 1:nrow(data_rows)) {
    row_string <- data_to_string[loop]

    duplicate_table <- data.table(colname = non_key_cols, value =  as.matrix(data_rows[loop, non_key_cols, with = FALSE])[1,])
    duplicate_table[, string_sql := paste0(colname, " = \'", value, "\'")]
    duplicate_string <- paste0(duplicate_table[, string_sql],  collapse = ", ")


    dbFetch(dbSendQuery(con, paste0('insert into ',
                                    table,
                                    ' (',
                                    colnames,
                                    ')',
                                    ' VALUES (\'',
                                    row_string,
                                    '\')',
                                    'ON DUPLICATE KEY UPDATE ',
                                    duplicate_string)))

  }

}
