
finish_turns_db <- function(con, ADM_OPTIMAL_MOVES, game_status, cycler_deck_status, pre_aggr_game_status, cycler_at_slot) {
  #cycler_id only needed for parsing the deck_left
  #cycler_at_slot <- 20
  #cycler_deck_status <- deck_status[CYCLER_ID == 5]


  #deck_copied_all <- copy(cycler_deck_status)
  deck_copied <- cycler_deck_status[Zone != "Removed"]
  final_result_list <- NULL
  #db handling, memory
  #keep all data in memory
  #if new row, then save to db

  #ADM_OPTIMAL_MOVES <- data.table(DECK_LEFT = "9993322", TRACK_LEFT = "NNNNNNNNNNNMMMAANNNNNNNNNN", TURNS_TO_FINISH = 3)
  #cycler_at_slot <- 20
  #create track left

  track_tot <- pre_aggr_game_status$aggr_to_slots
  finish_slot <- track_tot[FINISH == 1, GAME_SLOT_ID]
  track_left <- track_tot[GAME_SLOT_ID >= cycler_at_slot & GAME_SLOT_ID <= finish_slot, paste0(PIECE_ATTRIBUTE, collapse = "")]
  deck_left <- deck_copied[order(-MOVEMENT)][, paste0(MOVEMENT, collapse = "")]
  res_data_row <- data.table(TRACK_LEFT = track_left, DECK_LEFT = deck_left)
  #check if I have this observation

  row_result <- ADM_OPTIMAL_MOVES[res_data_row, on = .(TRACK_LEFT, DECK_LEFT)][!is.na(TURNS_TO_FINISH)]


      if (nrow(row_result) == 0){
        #no we dont have it, lets create it

        turns_to_finish_calc <- optimal_moves_to_finish(deck_copied,
                                                        cycler_at_slot,
                                                        pre_aggr_game_status,
                                                        use_draw_odds = FALSE)
        new_result_row <- cbind(res_data_row, turns_to_finish_calc)


        tryIns <- tryCatch({
          dbIns("ADM_OPTIMAL_MOVES", new_result_row, con)
          ADM_OPTIMAL_MOVES <- rbind(ADM_OPTIMAL_MOVES, new_result_row)
          FALSE
        }, error = function(e) {
          warning("tried to insert duplicate to ADM_OPTIMAL")
        })
        # if (tryIns == TRUE) {
        #   browser()
        #   dbIns("ADM_OPTIMAL_MOVES", new_result_row, con)
        #   ADM_OPTIMAL_MOVES
        # }

        final_result_list$turns_to_finish <- new_result_row[, TURNS_TO_FINISH]
        final_result_list$new_ADM_OPT <- ADM_OPTIMAL_MOVES
      } else {
        #we had that one already, return original

        final_result_list$turns_to_finish <- row_result[, TURNS_TO_FINISH]
        final_result_list$new_ADM_OPT <- ADM_OPTIMAL_MOVES
      }
  return(final_result_list)
}
