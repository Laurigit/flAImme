select_min_card_value_with_same_movement <- function(cycler_id, played_movement, turn_options, game_state) {

  max_move <- min(max(slots_to_mountain(game_state, cycler_id), 5), 9)
  min_move <- ifelse(game_state[CYCLER_ID == cycler_id, PIECE_ATTRIBUTE] == "A", 5,
                     ifelse(game_state[CYCLER_ID == cycler_id, PIECE_ATTRIBUTE] == "S", 4, 2))
  copy_opt <- copy(turn_options)
  copy_opt[, actual_movement := min(max(MOVEMENT, min_move), max_move), by = row_id]
  selected_actual_movemet <- copy_opt[MOVEMENT == played_movement, min(actual_movement)]
  played_card <- copy_opt[actual_movement == selected_actual_movemet, min(CARD_ID)]

  return(played_card)
  #if played movement <= min movement, select smallest movement
  #if played movement > max movement, select smallesst movement which is higher than max movement

}
