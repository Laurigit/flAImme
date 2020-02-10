
cyclers_turns_MOVEMEMENT_combs <- function(ctM_data, game_status, deck_status, turn_id, pre_aggr_game_status, extra_vector = NULL) {

  #extra vector format c(CYCLER_ID, MOVEMENT)
  #t <- c("CYCLER_ID", "MOVEMENT")
  #extra_vector <-  c(1,5)
  used_game_status <- copy(game_status)
  deck_copied <- copy(deck_status)
  #check if we already have data
  if (is.null(ctM_data)) {
    options <- data.table(dummy_row = 1)
    options <- options[1 == 0]
    } else {
  options <- ctM_data[TURN_ID == turn_id]
    }
  if (nrow(options) > 0) {
    #we have data, check if we have been requested an extra combination
    if (!is.null(extra_vector)) {
      #check if we have that already!
      row_count <- nrow(options[CYCLER_ID == extra_vector[1] & MOVEMENT == extra_vector[2]])
      if (row_count == 0){
        #no we dont have it, lets create it
        turns_to_finish_extra <- optimal_moves_to_finish(extra_vector[1], used_game_status, deck_copied, extra_vector[2], use_draw_odds = FALSE, pre_aggr_game_status)[, .(MOVEMENT = extra_vector[2], CYCLER_ID = cycler_id, turns_to_finish = Turns_to_finish, N = 0)]
        turns_to_finish_extra[, TURN_ID := turn_id]
        ctM_data <- rbind(ctM_data, turns_to_finish_extra)
      } else {
        #we had that one already, return original
        return(ctM_data)
      }

    } else {
      #nothing requested, return orig data
      return(ctM_data)
    }
  } else {
    #we dont have data for this turn, lets calc it
    not_played <- deck_copied[Zone != "Removed"]
    #this can be improved to use the known information when exhaust has been added
    options <- not_played[, .N, by = .(MOVEMENT, CYCLER_ID)][order(CYCLER_ID)]
    options[, turns_to_finish := 100]
    pre_res <- precalc_track(used_game_status)

    for (opt_loop in 1:nrow(options)) {
      options[opt_loop, turns_to_finish := optimal_moves_to_finish(CYCLER_ID, used_game_status, deck_status, MOVEMENT, use_draw_odds = FALSE, pre_res)[, Turns_to_finish]]
    }
    options[, TURN_ID := turn_id]
    ctM_data <- options
    return(ctM_data)
  }

}
