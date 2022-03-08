# c("TURNS_TO_FINISH", "SLOTS_OVER_FINISH", "NEXT_MOVE")
# := (finish_turns_db(con, TRACK_LEFT, DECK_LEFT, pre_aggr_game_status_no_list, NEW_GAME_SLOT_ID,
#                     draw_odds_raw_data = DRAW_ODDS, save_to_DB = TRUE))
add_ttf_multicore <- function(con, input_data_all, pre_aggr_game_status) {


  track_tot <- pre_aggr_game_status
  finish_slot <- track_tot[FINISH == 1, GAME_SLOT_ID]
  input_data_all[, IS_FINISHED := ifelse(NEW_GAME_SLOT_ID >= finish_slot, 1, 0)]

  if (nrow(input_data_all) > 0) {
  input_data <- input_data_all[, .N, by =  .(DECK_LEFT, NEW_GAME_SLOT_ID, DRAW_ODDS, TRACK_LEFT, IS_FINISHED)]
  input_data[, parse_draw_odds := ifelse(DRAW_ODDS == "", "", str_split(DRAW_ODDS, ";"))]
  input_data[, ':=' (draw_odds_input2 = list(ifelse(DRAW_ODDS == "", "",
                                                    data.table(Turn_to_Draw = as.numeric(str_split(parse_draw_odds[[1]][1], "")[[1]]),
                                                               MOVEMENT =  as.numeric(str_split(parse_draw_odds[[1]][2], "")[[1]]),
                                                               prob = -1))))]





  #finish_slot <- track_tot[FINISH == 1, GAME_SLOT_ID]



  # row_result <- ADM_OPTIMAL_MOVES[input_data, on = .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS)]
  # if (track_left_input != "") {


  #  if (nrow(row_result) == 0){
  #no we dont have it, lets create it
  to_calulcation <- input_data[IS_FINISHED == 0]


  klusteri <- (parallel::makeCluster(global_cores))
  registerDoParallel(klusteri)


  #foreach::getDoParRegistered()
  result <- foreach(i = 1:nrow(to_calulcation), .combine = 'rbind',
                    .packages = c("data.table", "stringr", "ompr", "ROI", "ROI.plugin.symphony",
                             "ompr.roi")) %dopar% {
                                source("./scripts/solution/optimal_moves_to_finish.R")
                                source("./scripts/solution/boundi_filtteri.R")
                                turns_to_finish_calc <- optimal_moves_to_finish(cycler_deck_status = to_calulcation[i, DECK_LEFT],
                                                                                calc_from_slot = to_calulcation[i, NEW_GAME_SLOT_ID],
                                                                                precalc_data = pre_aggr_game_status,
                                                                                use_draw_odds = to_calulcation[i, DRAW_ODDS])
                                turns_to_finish_calc
                              }
#  print(difftime(Sys.time(), timenow))
  stopCluster(klusteri)

  calc_res <- cbind(to_calulcation, result)

  no_calc <- input_data[IS_FINISHED == 1]
  no_calc[, ':=' (NEXT_MOVE = 5, TURNS_TO_FINISH = 0, SLOTS_OVER_FINISH = NEW_GAME_SLOT_ID - finish_slot)]
  join_res <- rbind(calc_res, no_calc)
 # join_res[, NEW_GAME_SLOT_ID := NULL]
#browser()
 # ADM_OPTIMAL_MOVES[join_res, on = .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS)]

  # turns_to_finish_calc <- optimal_moves_to_finish(cycler_deck_status,
  #                                                 cycler_at_slot,
  #                                                 pre_aggr_game_status,
  #                                                 use_draw_odds = draw_odds_input)
  # new_result_row <- data.table(TRACK_LEFT = track_left_input, DECK_LEFT = cycler_deck_status,
  #                              DRAW_ODDS = draw_odds_raw_data, turns_to_finish_calc)
 # join_res[, NEW_GAME_SLOT_ID := NULL]



  tryIns <- tryCatch({

     new_result_row <- join_res[, .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS, TURNS_TO_FINISH, SLOTS_OVER_FINISH,
                                    NEXT_MOVE)]
    dbIns("ADM_OPTIMAL_MOVES", new_result_row, con)
    # joinaa <- ADM_OPTIMAL_MOVES[new_result_row, on = .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS)]

    # if (nrow(joinaa) > 0) {

    ADM_OPTIMAL_MOVES <<- rbind(ADM_OPTIMAL_MOVES, new_result_row)
    #  print(paste0("in function ", nrow(ADM_OPTIMAL_MOVES)))
    #  }

    FALSE
  }, error = function(e) {

    warning("tried to insert duplicate to ADM_OPTIMAL")

  })
  # if (tryIns == TRUE) {
  #   browser()
  #   dbIns("ADM_OPTIMAL_MOVES", new_result_row, con)
  #   ADM_OPTIMAL_MOVES
  # }

  final_result_list <- new_result_row#[, TURNS_TO_FINISH]
  # } else {
  #   #we had that one already, return original
  #
  #   final_result_list <- row_result#[, TURNS_TO_FINISH]
  # }
  # } else {
  #
  #   slots_over <- cycler_at_slot -  finish_slot
  #   final_result_list <- data.table(TRACK_LEFT = track_left_input, DECK_LEFT = cycler_deck_status,
  #                                   DRAW_ODDS = draw_odds_raw_data, TURNS_TO_FINISH = 0,
  #                                   SLOTS_OVER_FINISH = slots_over,
  #                                   NEXT_MOVE = 0)
  # }
  final_result_list <- final_result_list[, .(TURNS_TO_FINISH = as.integer(TURNS_TO_FINISH),
                                             SLOTS_OVER_FINISH = as.integer(SLOTS_OVER_FINISH),
                                             NEXT_MOVE = as.integer(NEXT_MOVE),
                                             TRACK_LEFT, DECK_LEFT, DRAW_ODDS)]
  } else {
    final_result_list <- data.table(TURNS_TO_FINISH = as.integer(),
                                               SLOTS_OVER_FINISH = as.integer(),
                                               NEXT_MOVE = as.integer(),
                                               TRACK_LEFT = as.character(), DECK_LEFT = as.character(), DRAW_ODDS = as.character())
  }
  return(final_result_list)
}
