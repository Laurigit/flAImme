
#prepare most likely needed optimal moves
cyclers_turns_MOVEMEMENT_combs <- function(con, ADM_OPTIMAL_MOVES, game_status, deck_status, pre_aggr_game_status) {

  #what = precalculates turns

  #extra vector format c(CYCLER_ID, MOVEMENT, NEW_LANDING_SLOT) #so the the idea is that you can play card 5 but move actually 6 due tie slipstream
  #t <- c("CYCLER_ID", "MOVEMENT")
  #extra_vector <-  c(1,5)
  used_game_status <- copy(game_status)
  deck_copied <- copy(deck_status)
  curr_posits <- used_game_status[CYCLER_ID > 0, .(CYCLER_ID, curr_pos = GAME_SLOT_ID)]


    #we dont have data for this turn, lets calc it
    not_played <- deck_copied[Zone != "Removed"]
    #this can be improved to use the known information when exhaust has been added
    options <- not_played[, .N, by = .(MOVEMENT, CYCLER_ID)][order(CYCLER_ID)]
    options[, turns_to_finish := 100]

    #join current position
    join_curr <- curr_posits[options, on = "CYCLER_ID"]
    join_curr[, new_slot_after_moving := move_cycler(used_game_status, CYCLER_ID, MOVEMENT,
                                        slipstream = FALSE,
                                        ignore_block = TRUE,
                                        return_numeric_position = TRUE), by = .(CYCLER_ID, MOVEMENT)]




    for (opt_loop in 1:nrow(join_curr)) {
      loop_cycler <- join_curr[opt_loop, CYCLER_ID]
      cycler_deck_updated <- deck_copied[CYCLER_ID == join_curr[opt_loop, CYCLER_ID]]
      played_card <- join_curr[opt_loop, MOVEMENT]
      min_row_id_played <- cycler_deck_updated[Zone != "Removed", min(row_id)]
      cycler_deck_updated[row_id == min_row_id_played, Zone := "Removed"]


      slot <- join_curr[opt_loop, new_slot_after_moving]
      res_temp <- finish_turns_db(con, ADM_OPTIMAL_MOVES, used_game_status, cycler_deck_updated, pre_aggr_game_status, slot)
      ADM_OPTIMAL_MOVES <- res_temp$new_ADM_OPT
      join_curr[opt_loop, turns_to_finish := res_temp$turns_to_finish]
  }

    join_curr[, actual_movement := new_slot_after_moving - curr_pos]
    res_list <- NULL
    res_list$new_ADM_OPT <- ADM_OPTIMAL_MOVES
    res_list$ctM_data <- join_curr
    return(res_list)

}
