update_dt_values <- function(to_be_updated, new_data, join_cols, update_cols) {


  #update_cols <- c("TURNS_TO_FINISH", "NEXT_MOVE", "SLOTS_OVER_FINISH")
  copy_new <- copy(new_data)
  new_col_names <- paste0("new_", update_cols)
  setnames(copy_new, update_cols, new_col_names )
 # colnames(new_data) <- new_col_names
  joinaa <- copy_new[to_be_updated, on = c(join_cols)]

  for (coal_loop in update_cols) {
    setnames(joinaa, paste0("new_", coal_loop), "new_val")
    setnames(joinaa, coal_loop, "old_col")
    joinaa[, old_col := ifelse(is.na(old_col), new_val, old_col)]
    joinaa[, new_val := NULL]
    setnames(joinaa, "old_col", coal_loop)


  }

return(joinaa)
}
