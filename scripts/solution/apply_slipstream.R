#apply slipstream

#game_status <- start_game(input_STARTUP_DATA, 2, STG_TRACK_PIECE, STG_TRACK)
#game_state <- game_status
apply_slipstream <- function(game_state) {

  potential_cyclers <- game_state[!PIECE_ATTRIBUTE %in% c("M", "C") & CYCLER_ID > 0, .N, by = GAME_SLOT_ID][order(-GAME_SLOT_ID)]
  potential_cyclers[, diff := GAME_SLOT_ID - shift(GAME_SLOT_ID )]

  #move last 1 or 2 first
  slipstreamer_slots <- suppressWarnings(potential_cyclers[diff == -2, .(GAME_SLOT_ID = min(GAME_SLOT_ID))])

  if (nrow(slipstreamer_slots) > 0) {
    slip_cyclers <- game_state[CYCLER_ID > 0 & GAME_SLOT_ID == slipstreamer_slots[, GAME_SLOT_ID]][order(-SQUARE_ID)][, CYCLER_ID]
    for(slip_loop in slip_cyclers) {
      game_state <- move_cycler(game_state, slip_loop, 1, slipstream = TRUE)
     # print(paste0("SLIPSTREAM CYCLER ", slip_loop))
    }
    game_state <- apply_slipstream(game_state)
  }
  return(game_state)
}
