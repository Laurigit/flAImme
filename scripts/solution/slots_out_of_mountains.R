slots_out_of_mountains <- function(game_state) {
  # game_state[, rleidi := rleid(CYCLER_ID)]

  sscols <- game_state[, .(mountain_row = ifelse(PIECE_ATTRIBUTE == "M", 1, 0), GAME_SLOT_ID, CYCLER_ID)]
  sscols_aggr <- sscols[, .N, by = .(mountain_row, GAME_SLOT_ID)][order(-GAME_SLOT_ID)]
  sscols_aggr[, start_of_restricted_movement := ifelse(shift(mountain_row == 1, n = 6) | mountain_row == 1, 1, 0)]
  sscols_aggr[, start_of_restricted_movement := ifelse(is.na(start_of_restricted_movement), 0, start_of_restricted_movement)]
  sscols_aggr[, counter_cons_piece := rowid(rleid(start_of_restricted_movement))]
  sscols_aggr[, turns_ouf_of_mountain_temp := ifelse(start_of_restricted_movement == 1, ceiling(counter_cons_piece / 5), NA)]
  sscols_aggr[, turns_ouf_of_mountain_locf := na.locf(turns_ouf_of_mountain_temp, fromLast = FALSE, na.rm = FALSE)]
  sscols_aggr[, turns_out_of_mountain := ifelse(is.na(turns_ouf_of_mountain_locf), 0, turns_ouf_of_mountain_locf)]
  sscols_res <- sscols_aggr[, .(GAME_SLOT_ID, turns_out_of_mountain)]
  joinaa <- sscols_res[game_state, on = "GAME_SLOT_ID"]

  return(joinaa)
}
