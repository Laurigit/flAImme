#game_state <- game_status
getDownHillMoves <- function(game_state, cycler_id) {
  cyclerPos <- game_state[CYCLER_ID == cycler_id, GAME_SLOT_ID]
  possible_downhill_landings <- game_state[GAME_SLOT_ID >= cyclerPos + 2 & GAME_SLOT_ID <= cyclerPos + 9 & PIECE_ATTRIBUTE == "A", GAME_SLOT_ID - cyclerPos]
if (length(possible_downhill_landings) == 0) {
  result <- 0
} else {
    result <- possible_downhill_landings
}
  return(result)
}
