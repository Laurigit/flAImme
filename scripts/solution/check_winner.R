
#winner_state <- default_row

check_winner <- function(game_state, winner_state = NULL, turn_id, game_id) {
  if (is.null(winner_state)) {
    winner_state <- data.table(TURN_ID = numeric(), CYCLER_ID = numeric(), POSITION = numeric(), GAME_ID = numeric(),
                              row_over_finish = numeric(), finish_square = numeric())
  }

  finish_lane <-game_state[FINISH == 1, max(GAME_SLOT_ID)]
  finish_slots <- game_state[GAME_SLOT_ID >= finish_lane]
  #any there?
  finishers <- finish_slots[order(-SQUARE_ID)][CYCLER_ID > 0]
  if (nrow(finishers) > 0) {
    #next free position

    if (nrow(winner_state) > 0) {
      position_counter <- winner_state[, max(POSITION)]
    } else {
      position_counter <- 0
    }

    for(finish_loop in finishers[, CYCLER_ID]) {
      position_counter <- position_counter + 1

      result_row <- data.table(TURN_ID = turn_id, CYCLER_ID = finish_loop, POSITION = position_counter, GAME_ID = game_id,
                               row_over_finish = finishers[CYCLER_ID == finish_loop, max(GAME_SLOT_ID)] - finish_lane,
                               finish_square = finishers[CYCLER_ID == finish_loop, SQUARE_ID])
      winner_state <- rbind(winner_state, result_row)
    }

  }
  return(winner_state)
}
