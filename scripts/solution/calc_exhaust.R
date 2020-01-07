calc_exhaust <- function(game_state) {

  potential_cyclers <- game_state[ CYCLER_ID > 0, .N, by = GAME_SLOT_ID][order(-GAME_SLOT_ID)]
  potential_cyclers[, diff := GAME_SLOT_ID - shift(GAME_SLOT_ID )]

  exhaust_slots <- potential_cyclers[diff != -1 | is.na(diff) , GAME_SLOT_ID]

  exhaust_cyclers <- game_state[CYCLER_ID > 0, .(CYCLER_ID, EXHAUST = ifelse(GAME_SLOT_ID %in% exhaust_slots, 1, 0))]
  return(exhaust_cyclers)
}
