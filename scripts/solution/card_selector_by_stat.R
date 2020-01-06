
#game_state <- game_status
card_selector_by_stat <- function(game_state, turn_options, cycler_id, strategy,  aim_downhill = FALSE) {
  max_move <- max(slots_to_mountain(game_state, cycler_id), 5)
  min_move <- ifelse(game_state[CYCLER_ID == cycler_id, PIECE_ATTRIBUTE] == "A", 5, 2)
  if (aim_downhill == TRUE) {

    potential_dh_moves <- getDownHillMoves(game_state, cycler_id)
    #potential_dh_moves<- 2
    #check if they are available
    avail_dh_moves <- turn_options[MOVEMENT %in% potential_dh_moves]

    if (nrow(avail_dh_moves) > 0) {
      turn_options <- avail_dh_moves
    }
  }



  if (strategy == "MAX") {
  try_max_move  <- suppressWarnings(turn_options[MOVEMENT >= max_move, min(MOVEMENT)])
  if (is.infinite(try_max_move)) {
    try_max_move <- turn_options[, max(MOVEMENT)]
  }
  #if move is 5 or less and we are on downhill, switch to smallest

  if (min_move >= try_max_move) {
    #min_card_avail
    min_card_avail <- turn_options[, min(MOVEMENT)]

    get_played_row_id <- turn_options[MOVEMENT == min_card_avail, min(row_id)]
  } else {
    get_played_row_id <- turn_options[MOVEMENT == try_max_move, min(row_id)]
  }
  result <- turn_options[row_id == get_played_row_id, .(MOVEMENT, row_id, actual_movement = max(min(max_move, MOVEMENT), min_move))]
  return(result)
  } else if (strategy == "SMART_MAX") {

    try_max_move  <- turn_options[MOVEMENT <= max_move, max(MOVEMENT)]

    if (min_move >= try_max_move) {
      #min_card_avail
      min_card_avail <- turn_options[, min(MOVEMENT)]

      get_played_row_id <- turn_options[MOVEMENT == min_card_avail, min(row_id)]
    } else {
      get_played_row_id <- turn_options[MOVEMENT == try_max_move, min(row_id)]
    }
    result <- turn_options[row_id == get_played_row_id, .(MOVEMENT, row_id, actual_movement = max(min(max_move, MOVEMENT), min_move))]
    return(result)
  }
}
