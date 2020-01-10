# aim_downhill_count = 0
# strategy <- "MAX"
simulate_strategy <- function(game_state, deck_status, cycler_id, strategy, aim_downhill_count = 0, card_options) {
  temp_state <- game_state[1 != 0, .(PIECE_ATTRIBUTE, FINISH, GAME_SLOT_ID, SQUARE_ID, CYCLER_ID)]
  orig_dh_start <- aim_downhill_count
  actions <- NULL

  for (simul_turn in 1:30) {

    turn_options <- card_options[turn_simulate == simul_turn]
    if (nrow(turn_options) == 0) {
      turn_options <- card_options[turn_simulate == 0]
    }
    #play smallest card that is greater or equal to max move
    if (aim_downhill_count <= 0) {
      input_aim_downhill <- FALSE
    } else {
      input_aim_downhill <- TRUE
    }

    selected_card <- card_selector_by_stat(temp_state, turn_options, cycler_id, strategy , aim_downhill = input_aim_downhill)

    #check if we hit downhill
    if (selected_card[, MOVEMENT] < selected_card[, actual_movement]) {
      aim_downhill_count <- aim_downhill_count - 1
    }

    action_row <- cbind(selected_card, STRAT_ID = strategy, downhill_start = orig_dh_start)
    actions <- rbind(actions, action_row)
   #a print(actions)
    card_options <- card_options[row_id != action_row[, row_id]]
    temp_state <- move_cycler(temp_state, cycler_id, action_row[, MOVEMENT], ignore_block = TRUE)
    winner_state <- check_winner(temp_state, winner_state, turn_id = simul_turn, game_id = 1)
    if (nrow(winner_state) > 0) {
      break()
    }

  }

  result <- NULL
  result$score <- winner_state
  result$actions <- actions
return(result)
}
