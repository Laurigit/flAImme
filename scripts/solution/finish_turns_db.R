
finish_turns_db <- function(con, track_left_input, cycler_deck_status, pre_aggr_game_status, cycler_at_slot,
                            draw_odds_raw_data = NULL, save_to_DB = TRUE) {
  #cycler_id only needed for parsing the deck_left
  #cycler_at_slot <- 20
  #cycler_deck_status <- deck_status[CYCLER_ID == 5]

  #draw odds are in data table format and sql db format. raw = db. We use raw in joins and data.table to be sent to optimization
   if (draw_odds_raw_data == "") {
    draw_odds_input <- ""
  } else {

    parse_draw_odds <- str_split(draw_odds_raw_data, ";")
    draw_odds_input <- data.table(Turn_to_Draw = as.numeric(str_split(parse_draw_odds[[1]][1], "")[[1]]),
                                  MOVEMENT =  as.numeric(str_split(parse_draw_odds[[1]][2], "")[[1]]),
                                  prob = -1)

  }

  #deck_copied_all <- copy(cycler_deck_status)

  final_result_list <- NULL
  #db handling, memory
  #keep all data in memory
  #if new row, then save to db

  #ADM_OPTIMAL_MOVES <- data.table(DECK_LEFT = "9993322", TRACK_LEFT = "NNNNNNNNNNNMMMAANNNNNNNNNN", TURNS_TO_FINISH = 3)
  #cycler_at_slot <- 20
  #create track left

  track_tot <- pre_aggr_game_status$aggr_to_slots
  finish_slot <- track_tot[FINISH == 1, GAME_SLOT_ID]

  #check if I have this observation

  row_result <- ADM_OPTIMAL_MOVES[TRACK_LEFT == track_left_input & DECK_LEFT == cycler_deck_status & DRAW_ODDS == draw_odds_raw_data]



      if (nrow(row_result) == 0){
        #no we dont have it, lets create it

        turns_to_finish_calc <- optimal_moves_to_finish(cycler_deck_status,
                                                        cycler_at_slot,
                                                        pre_aggr_game_status,
                                                        use_draw_odds = draw_odds_input)
        new_result_row <- data.table(TRACK_LEFT = track_left_input, DECK_LEFT = cycler_deck_status,
                                     DRAW_ODDS = draw_odds_raw_data, turns_to_finish_calc)


        tryIns <- tryCatch({

          new_result_row <- new_result_row[, .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS, TURNS_TO_FINISH )]
          dbIns("ADM_OPTIMAL_MOVES", new_result_row, con)
          FALSE
        }, error = function(e) {

          warning("tried to insert duplicate to ADM_OPTIMAL")

        })
        # if (tryIns == TRUE) {
        #   browser()
        #   dbIns("ADM_OPTIMAL_MOVES", new_result_row, con)
        #   ADM_OPTIMAL_MOVES
        # }

        final_result_list <- new_result_row[, TURNS_TO_FINISH]
      } else {
        #we had that one already, return original

        final_result_list <- row_result[, TURNS_TO_FINISH]
      }
  return(final_result_list)
}
