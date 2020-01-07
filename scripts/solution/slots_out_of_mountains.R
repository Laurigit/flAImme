slots_out_of_mountains <- function(game_state, cycler_id) {
  # game_state[, rleidi := rleid(CYCLER_ID)]
  sscols <- game_state[, .(mountain_row = ifelse(PIECE_ATTRIBUTE == "M", 1, 0), GAME_SLOT_ID, CYCLER_ID)]
  sscols[, counter_cons_piece := rowid(rleid(mountain_row))]
  # game_state[GAME_SLOT_ID >= last_cycler | PIECE_ATTRIBUTE == "M"]
  # game_state[CYCLER_ID > 0| PIECE_ATTRIBUTE == "M"]

  cycler_position <- game_state[CYCLER_ID == cycler_id, min(GAME_SLOT_ID)]
  #7th row confirms that area is long enough. Then reduce 6 to find the first row
  non_mountain_area <- suppressWarnings(sscols[mountain_row == 0 & counter_cons_piece >= 7 & GAME_SLOT_ID >= cycler_position, min(GAME_SLOT_ID) - 6])
  if (is.finite(non_mountain_area)) {
    distance_to_non_mountain <- non_mountain_area - cycler_position
  } else {
    distance_to_non_mountain <- NULL
  }
  return(distance_to_non_mountain)
}
