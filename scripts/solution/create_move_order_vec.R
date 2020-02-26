create_move_order_vec <- function(game_status, moveing_cyclers) {
  move_order <- game_status[CYCLER_ID > 0, .(SQUARE_ID), by = CYCLER_ID]
  join_phase <- move_order[CYCLER_ID %in% moveing_cyclers][order(-SQUARE_ID), CYCLER_ID]
  return(join_phase)
}
