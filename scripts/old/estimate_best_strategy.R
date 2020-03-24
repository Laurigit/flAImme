#cycler_id <- 2
#game_state <- game_status
estimate_best_strategy <- function(game_state, deck_status, strategy, cycler_id) {


  #temp_state[CYCLER_ID > 0]
  #deck_status[CYCLER_ID == cycler_id]
  winner_state <- NULL
   temp_state <- game_state[1 != 0, .(PIECE_ATTRIBUTE, FINISH, GAME_SLOT_ID, SQUARE_ID, CYCLER_ID)]
   card_options <- simulate_get_card_options(deck_status, cycler_id)
   result_rows <- NULL
   prev_KPI <- 100000
  #start smart_max and check how downhills needed
   dh_counter <- 0
   for(resloop in 1:10) {

     result_one <- simulate_strategy(temp_state, deck_status, cycler_id, "SMART_MAX", aim_downhill_count = dh_counter, card_options)
         KPI <- result_one$score[, TURN_ID * 1000 - row_over_finish]

    if (KPI >= prev_KPI) {
      dh_counter <- dh_counter - 1
      break()
    }
    prev_KPI <- KPI
    result_prev <- result_one
    dh_counter <- dh_counter + 1
   }
   #then max
   maxRes <-  simulate_strategy(temp_state, deck_status, cycler_id, "MAX", aim_downhill_count = dh_counter, card_options)
   KPI_max <- maxRes$score[, TURN_ID * 1000 - row_over_finish]

   if (KPI_max < prev_KPI) {
   best_stat <- "MAX"
   best_actions <- maxRes
   } else {
     best_stat <- "SMART_MAX"
     best_actions <- result_prev
   }

  speed_pct <-  cbind(best_actions$score[, .(row_over_finish, finish_square, TURN_ID)], best_actions$actions[, .(speed = sum(actual_movement)/sum(max_move), strategy = best_stat, dh_count = dh_counter, CYCLER_ID = cycler_id)])


return(speed_pct)
}
