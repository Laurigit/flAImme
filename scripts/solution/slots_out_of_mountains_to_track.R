slots_out_of_mountains_to_track <- function(game_state) {
  # game_state[, rleidi := rleid(CYCLER_ID)]

  sscols <- game_state[, .(mountain_row = ifelse(PIECE_ATTRIBUTE == "M", 1, 0), GAME_SLOT_ID, CYCLER_ID)]
  sscols_aggr <- sscols[, .N, by = .(mountain_row, GAME_SLOT_ID)][order(-GAME_SLOT_ID)]
 # sscols_aggr[, start := ifelse(shift(mountain_row == 1, n = 6) | mountain_row == 1, 1, 0)]
  #sscols_aggr[, start_of_restricted_movement := ifelse(is.na(start_of_restricted_movement), 0, start_of_restricted_movement)]
  sscols_aggr[, counter_cons_piece := rowid(rleid(mountain_row)) - 1]
  sscols_aggr[, max_move := ifelse(mountain_row == 1, 5, pmax(counter_cons_piece, 5))]
 sscols_res <- sscols_aggr[, .(MAXIMUM_MOVEMENT = max_move, GAME_SLOT_ID)]
 joinaa <- sscols_res[game_state, on = .(GAME_SLOT_ID)]
  return(joinaa)
}
