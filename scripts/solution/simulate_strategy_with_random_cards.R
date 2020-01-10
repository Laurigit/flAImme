# aim_downhill_count = 0
# strategy <- "MAX"
simulate_strategy_with_random_cards <- function(game_state, deck_status, cycler_id, strategy, aim_downhill_count = 0) {
  temp_state <- game_state[1 != 0, .(PIECE_ATTRIBUTE, FINISH, GAME_SLOT_ID, SQUARE_ID, CYCLER_ID)]
  deck_status_temp <- deck_status[CYCLER_ID == cycler_id]
  orig_dh_start <- aim_downhill_count



  for (simul_turn in 1:40) {

    deck_status_temp <- draw_cards(cycler_id, deck_status_temp, how_many_cards = 4, FALSE)
    turn_options <- deck_status_temp[Zone == "Hand" & CYCLER_ID == cycler_id, .(MOVEMENT, row_id)]



    #play smallest card that is greater or equal to max move
    if (aim_downhill_count <= 0) {
      input_aim_downhill <- FALSE
    } else {
      input_aim_downhill <- TRUE
    }

    selected_card <- card_selector_by_stat(temp_state, turn_options, cycler_id , aim_downhill = input_aim_downhill)

    #check if we hit downhill
    if (selected_card[, MOVEMENT] < selected_card[, actual_movement]) {
      aim_downhill_count <- aim_downhill_count - 1
    }



    temp_state <- move_cycler(temp_state, cycler_id, selected_card[, MOVEMENT], ignore_block = TRUE)

    winner_state <- check_winner(temp_state, winner_state, turn_id = simul_turn, game_id = 1)
    if (nrow(winner_state) > 0) {
      break()
    }
    deck_status_temp <- play_card(cycler_id, card_id =  NULL, deck_status_temp, game_id = 1, turn_id = simul_turn, con = FALSE, card_row_id = selected_card[, row_id])
  }


  result <- winner_state
  #result$actions <- actions
  return(result)
}
