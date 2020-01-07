#cycler_id <- 2
#game_state <- game_status
turns_to_finish <- function(game_state, deck_status, strategy, cycler_id) {


  temp_state[CYCLER_ID > 0]
  deck_status[CYCLER_ID == cycler_id]
  winner_state <- NULL
   temp_state <- game_state[1 != 0, .(PIECE_ATTRIBUTE, FINISH, GAME_SLOT_ID, SQUARE_ID, CYCLER_ID)]
   card_options <- simulate_get_card_options(deck_status, cycler_id)
  res <- simulate_strategy(temp_state, deck_status, cycler_id, "MAX", aim_downhill_count = 0, card_options)
  res4 <- simulate_strategy(temp_state, deck_status, cycler_id, "MAX", aim_downhill_count = 4, card_options)
  res2 <- simulate_strategy(temp_state, deck_status, cycler_id, "SMART_MAX", aim_downhill_count = 0, card_options)
  res3 <- simulate_strategy(temp_state, deck_status, cycler_id, "SMART_MAX", aim_downhill_count = 4, card_options)


  action_total <- rbind(action_total, actions3)

  res_data <- NULL
  res_data$finish <-  winner_state[, .(TURN_ID, STRAT_ID = GAME_ID, row_over_finish, finish_square)]
  res_data$action <- action_total

return(res_data)
}
