push_cycler_down <- function(game_status, cycler_id) {
  #check current slot

  occupied_square <- game_status[CYCLER_ID == cycler_id, SQUARE_ID]
  pushed_square <- game_status[CYCLER_ID == cycler_id, SQUARE_ID] - 1
  #check if it is free
  who_occupies_square <- game_status[SQUARE_ID == pushed_square, CYCLER_ID]
  if (who_occupies_square > 0) {
    push_cycler_down(game_status, who_occupies_square)
  }
  game_status[SQUARE_ID == occupied_square, CYCLER_ID := 0]
  game_status[SQUARE_ID == pushed_square, CYCLER_ID := cycler_id]
  return(game_status)
}
