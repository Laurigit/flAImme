#cycler_id <- 2
#game_state <- game_status
turns_to_finish <- function(game_state, deck_status) {



  cyclers <- game_state[CYCLER_ID > 0, CYCLER_ID]
  #temp_state[CYCLER_ID > 0]
  #deck_status[CYCLER_ID == cycler_id]

  temp_state <- game_state[1 != 0, .(PIECE_ATTRIBUTE, FINISH, GAME_SLOT_ID, SQUARE_ID, CYCLER_ID, EXTRA)]

   card_options <- rbindlist(lapply(cyclers, function(x) {simulate_get_card_options(deck_status, x)}))

#     result_one <- simulate_strategy(temp_state, deck_status, cycler_id, "SMART_MAX", aim_downhill_count = 0, card_options)
    # result_one <- rbindlist(lapply(cyclers, function(x) {simulate_strategy(temp_state, deck_status, x, "SMART_MAX", 0, card_options, return_ony_score = TRUE)}))


     result_two <- rbindlist(lapply(cyclers, function(x) {simulate_strategy(temp_state, deck_status, x, "SMART_MAX", 1, card_options, return_ony_score = TRUE)}))

     result_three <- rbindlist(lapply(cyclers, function(x) {simulate_strategy(temp_state, deck_status, x, "MAX", 0, card_options, return_ony_score = TRUE)}))

     #KPI_smart <- result_one$score[, TURN_ID * 1000 - row_over_finish]

  speed_pct <- rbind(result_two, result_three)


return(speed_pct)
}
