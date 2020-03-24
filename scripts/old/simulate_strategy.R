# aim_downhill_count = 0
# strategy <- "MAX"
#NOT SUPPLY FIXED
simulate_strategy <- function(game_state, deck_status, cycler_id, strategy, aim_downhill_count = 0, card_options, return_ony_score = FALSE) {
  temp_state <- game_state[1 != 0, .(PIECE_ATTRIBUTE, FINISH, GAME_SLOT_ID, SQUARE_ID, CYCLER_ID, EXTRA)]
  orig_dh_start <- aim_downhill_count
  actions <- NULL

  for (simul_turn in 1:100) {

    turn_options <- card_options[turn_simulate == simul_turn & CYCLER_ID == cycler_id]
    if (nrow(turn_options) == 0) {
      turn_options <- card_options[turn_simulate == 0]
      #add exhaust if no cards
      if (nrow(turn_options) == 0) {
        turn_options <- data.table(row_id = 0, MOVEMENT =2, CYCLER_ID = cycler_id, turn_simulate = 0)
      }
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
    card_options <- card_options[CYCLER_ID == cycler_id & row_id != action_row[, row_id]]

    temp_state <- move_cycler(temp_state, cycler_id, action_row[, MOVEMENT], ignore_block = TRUE)
    winner_state <- check_winner(temp_state, winner_state = NULL, turn_id = simul_turn, game_id = 1)
    if (nrow(winner_state) > 0) {

      break()
    }

  }

  speed <- actions[, .(Speed = sum(actual_movement / sum (max_move)), CYCLER_ID = cycler_id, strategy, aim_downhill_count)]
  if (return_ony_score == FALSE) {
  result <- NULL
  result$score <- winner_state
  result$actions <- actions
  } else {
    join_speed <- speed[winner_state, on = "CYCLER_ID"]
    result <-  join_speed
  }
return(result)
}
