
#game_state <- game_status
card_selector_by_stat <- function(game_state, turn_options, cycler_id, strategy,  aim_downhill = FALSE) {
  max_move <- min(max(slots_to_mountain(game_state, cycler_id), 5), 9)
  min_move <- ifelse(game_state[CYCLER_ID == cycler_id, PIECE_ATTRIBUTE] == "A", 5, 2)
  turn_options[, actual_movement := min(max(MOVEMENT, min_move), max_move), by = row_id]
  if (aim_downhill == TRUE) {

    potential_dh_moves <- getDownHillMoves(game_state, cycler_id)
    #potential_dh_moves<- 2
    #check if they are available
    avail_dh_moves <- turn_options[actual_movement %in% potential_dh_moves]

    if (nrow(avail_dh_moves) > 0) {
      turn_options <- avail_dh_moves
    }

    #if on downhill, use it if it useful

    if (min_move > turn_options[, min(MOVEMENT)]) {

      turn_options <- turn_options[MOVEMENT == turn_options[, min(MOVEMENT)]]
    }

  }

  if (strategy == "MAX") {
    try_max_move  <- suppressWarnings(turn_options[actual_movement == max_move, min(MOVEMENT)])
    if (is.infinite(try_max_move)) {
      try_max_move <- turn_options[, max(MOVEMENT)]
    }

    #pick smallest MOVEMENT with selected actual_movement
    get_played_row_id <- turn_options[MOVEMENT == try_max_move, min(row_id)]

    result <- turn_options[row_id == get_played_row_id, .(MOVEMENT, row_id, actual_movement = max(min(max_move, MOVEMENT), min_move))]
    result <- cbind(result, min_move, max_move)
    return(result)
  } else if (strategy == "SMART_MAX") {

    try_max_move  <- turn_options[actual_movement <= max_move, max(actual_movement)]
    #min MOVEMENT card
    min_movement  <- turn_options[actual_movement == try_max_move, min(MOVEMENT)]
    get_played_row_id <- turn_options[MOVEMENT == min_movement, min(row_id)]

    result <- turn_options[row_id == get_played_row_id, .(MOVEMENT, row_id, actual_movement = max(min(max_move, MOVEMENT), min_move))]
    result <- cbind(result, min_move, max_move)
    return(result)
    # } else if (strategy == "2nd") {
    #   my_pos <- game_state[order(-SQUARE_ID)][CYCLER_ID > 0,. (CYCLER_ID, seq_len(.N))][CYCLER_ID == cycler_id, V2]
    #   if (my_pos == 1) {
    #     my_slow_move <- turn_options[, min(MOVEMENT)]
    #     get_played_row_id <- turn_options[MOVEMENT == my_slow_move, min(row_id)]
    #
    #   } else {
    #     my_medium_move <- turn_options
    #   }
  }
}
