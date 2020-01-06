slots_to_mountain <- function(game_state, cycler_id) {

  # game_state[, rleidi := rleid(CYCLER_ID)]
  # game_state[GAME_SLOT_ID >= last_cycler | PIECE_ATTRIBUTE == "M"]
  # game_state[CYCLER_ID > 0| PIECE_ATTRIBUTE == "M"]
  cycler_position <- game_state[CYCLER_ID == cycler_id, min(GAME_SLOT_ID)]
  mountain_row <- suppressWarnings(game_state[PIECE_ATTRIBUTE == "M" & GAME_SLOT_ID >= cycler_position, min(GAME_SLOT_ID)])
  if (is.finite(mountain_row)) {
    distance_to_mountain <- mountain_row - cycler_position
  } else {
    distance_to_mountain <- 10000
  }
  return(distance_to_mountain)
}
