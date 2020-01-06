#clear_finishers_from_track
clear_finishers <- function(game_state, finishers) {
  #clear track
  game_state[CYCLER_ID %in% finishers, CYCLER_ID := 0]
  return(game_state)
}
