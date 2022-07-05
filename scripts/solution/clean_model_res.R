clean_model_res <- function(model_result, kortit) {

copy_res <- copy(model_result)

  raw <- as.data.table(copy_res, keep.rownames = TRUE)[copy_res > 0]
  if (nrow(raw) > 0) {
  res <- nrow(raw)
  raw[, c("i", "j", "k") := tstrsplit(rn, ",", fixed = TRUE)]
  raw[, i_clean := as.numeric(gsub("\\D", "", i))]
  raw[, j_clean := as.numeric(gsub("\\D", "", j))]
  raw[, k_clean := as.numeric(gsub("\\D", "", k))]
  setorder(raw, i_clean)
  res_kortit <- data.table(MOVEMENT = kortit, k_clean = 1:length(kortit))
  join_move <- res_kortit[raw, on = "k_clean"]
  final_res <- join_move[, .(MOVEMENT, i_clean, SLOTS_PROGRESSED = j_clean - i_clean, j_clean)]
  return(final_res)
} else {

  final_res <- data.table(MOVEMENT = -1, SLOTS_PROGRESSED = -1, i_clean = -1, j_clean = -1)
}
}
