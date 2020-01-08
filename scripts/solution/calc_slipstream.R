calc_slipstream <- function(game_state, SR_DATA = NULL) {

  if(is.null(SR_DATA)) {
    SR_DATA <- data.table( CYCLER_ID = numeric(), SLIP_ROWS = numeric(), slip_instance = numeric (), SR_GIVER_CYCLER_ID = numeric())
  }

  #game_state<-move_cycler(game_state,5, 1, slipstream = TRUE)
  #game_state[PIECE_ATTRIBUTE != "M" & CYCLER_ID > 0]
  potential_cyclers <- game_state[PIECE_ATTRIBUTE != "M" & CYCLER_ID > 0, .N, by = GAME_SLOT_ID][order(-GAME_SLOT_ID)]
  potential_cyclers[, diff := GAME_SLOT_ID - shift(GAME_SLOT_ID )]
  potential_cyclers[, diff_giver := GAME_SLOT_ID - shift(GAME_SLOT_ID, type = "lead" )]

  slipstreamer_slots <- suppressWarnings(potential_cyclers[diff == -2, .(GAME_SLOT_ID = min(GAME_SLOT_ID))])

  if (nrow(slipstreamer_slots) > 0) {
    slip_cyclers <- game_state[CYCLER_ID > 0 & GAME_SLOT_ID == slipstreamer_slots[, GAME_SLOT_ID], CYCLER_ID]
    slipstreamer_giver_slot <- suppressWarnings(potential_cyclers[diff_giver == 2, .(GAME_SLOT_ID = min(GAME_SLOT_ID))])
    slipstreamer_givers <- game_state[GAME_SLOT_ID == slipstreamer_giver_slot[, GAME_SLOT_ID] & CYCLER_ID > 0, .(SR_GIVER_CYCLER_ID = CYCLER_ID, key_col = 1)]

    for(slip_loop in slip_cyclers) {
      game_state <- move_cycler(game_state, slip_loop, 1, slipstream = TRUE)
      if(is.null(SR_DATA)) {
        slip_instance <- 1
      } else {
        slip_instance <- SR_DATA[, max(slip_instance)] + 1
      }
      sr_row <- data.table(CYCLER_ID = slip_loop, SLIP_ROWS = 1, key_col = 1, slip_instance)
      join_sr <- merge(sr_row, slipstreamer_givers, by = "key_col", all = TRUE)
      join_sr[, key_col := NULL]
      SR_DATA <- rbind(SR_DATA, join_sr)

      # print(paste0("SLIPSTREAM CYCLER ", slip_loop))
    }
    SR_DATA <- calc_slipstream(game_state, SR_DATA)
  }
  return(SR_DATA)
}
