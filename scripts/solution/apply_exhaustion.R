#APPLY_exhaustions


#game_status <- start_game(input_STARTUP_DATA, 2, STG_TRACK_PIECE, STG_TRACK)
#initial decks
#current_deck <- create_decks(input_STARTUP_DATA[, CYCLER_ID], ADM_CYCLER_DECK)


apply_exhaustion <- function(current_deck, game_state) {

  potential_cyclers <- game_state[ CYCLER_ID > 0, .N, by = GAME_SLOT_ID][order(-GAME_SLOT_ID)]
  potential_cyclers[, diff := GAME_SLOT_ID - shift(GAME_SLOT_ID )]

  exhaust_slots <- potential_cyclers[diff != -1 | is.na(diff) , GAME_SLOT_ID]

  exhaust_cyclers <- game_state[GAME_SLOT_ID %in% exhaust_slots & CYCLER_ID > 0, .(CYCLER_ID)]

  if (nrow(exhaust_cyclers) > 0) {
    ex_cycler <-exhaust_cyclers[, CYCLER_ID]

      current_deck <- add_exhaustion(ex_cycler, current_deck, "Recycle")
     #print(paste0("EXHAUST CYCLER ", ex_cycler))

  }
  return(current_deck)

}
