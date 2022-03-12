# c("TURNS_TO_FINISH", "SLOTS_OVER_FINISH", "NEXT_MOVE")
# := (finish_turns_db(con, TRACK_LEFT, DECK_LEFT, pre_aggr_game_status_no_list, NEW_GAME_SLOT_ID,
#                     draw_odds_raw_data = DRAW_ODDS, save_to_DB = TRUE))
add_ttf_multicore <- function(con, input_data_all, pre_aggr_game_status, game_slot_col_name = "NEW_GAME_SLOT_ID", db_handling = "SAVE") {
  # input_data_all <- join_known[]
  # pre_aggr_game_status <- pre_aggr_game_status_no_list
  # game_slot_col_name <- "NEW_GAME_SLOT_ID"
  # db_handling<- "SAVE"

  track_tot <- pre_aggr_game_status
  finish_slot <- track_tot[FINISH == 1, GAME_SLOT_ID]

setnames(input_data_all, game_slot_col_name, "NEW_GAME_SLOT_ID")
  input_data_all[, IS_FINISHED := ifelse(NEW_GAME_SLOT_ID >= finish_slot, 1, 0)]

  if (nrow(input_data_all) > 0) {
  input_data <- input_data_all[, .N, by =  .(DECK_LEFT, NEW_GAME_SLOT_ID, DRAW_ODDS, TRACK_LEFT, IS_FINISHED)]
  # input_data[, parse_draw_odds := ifelse(DRAW_ODDS == "", "", str_split(DRAW_ODDS, ";"))]
  # input_data[, ':=' (draw_odds_input2 = list(ifelse(DRAW_ODDS == "", "",
  #                                                   data.table(Turn_to_Draw = as.numeric(str_split(parse_draw_odds[[1]][1], "")[[1]]),
  #                                                              MOVEMENT =  as.numeric(str_split(parse_draw_odds[[1]][2], "")[[1]]),
  #                                                              prob = -1))))]





  #finish_slot <- track_tot[FINISH == 1, GAME_SLOT_ID]



  # row_result <- ADM_OPTIMAL_MOVES[input_data, on = .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS)]
  # if (track_left_input != "") {


  #  if (nrow(row_result) == 0){
  #no we dont have it, lets create it
  to_calulcation <- input_data[IS_FINISHED == 0]


if (nrow(to_calulcation) > 0) {
  len <- to_calulcation[1, nchar(TRACK_LEFT)]
  if ((len > 50 & nrow(to_calulcation) > 25) & global_dont_multicore == FALSE) {
  klusteri <- (parallel::makeCluster(global_cores))
  registerDoParallel(klusteri)


  #foreach::getDoParRegistered()

#if (global_dont_multicore == FALSE) {
#   print(to_calulcation)
  alku <- Sys.time()
  result <- foreach(i = 1:nrow(to_calulcation), .combine = 'rbind',
                    .packages = c("data.table", "stringr", "ompr", "ROI", "ROI.plugin.symphony",
                             "ompr.roi", "optiRum")) %dopar% {
                                source("./scripts/solution/optimal_moves_to_finish.R")
                                source("./scripts/solution/boundi_filtteri.R")

                                turns_to_finish_calc <- optimal_moves_to_finish(cycler_deck_status = to_calulcation[i, DECK_LEFT],
                                                                                calc_from_slot = to_calulcation[i, NEW_GAME_SLOT_ID],
                                                                                precalc_data = pre_aggr_game_status,
                                                                                draw_odds_raw_data = to_calulcation[i, DRAW_ODDS])

                                turns_to_finish_calc
                             }
  stopCluster(klusteri)
  sapply(file.path(tempdir(), list.files(tempdir())), unlink)
  kesto <- difftime(Sys.time(), alku)
 # kesto
  print(paste0("kesto multicore ", kesto))
} else {
  result <- NULL
  alku2 <- Sys.time()
  for (loop_calc in 1:nrow(to_calulcation)) {
    turns_to_finish_calc <- optimal_moves_to_finish(cycler_deck_status = to_calulcation[loop_calc, DECK_LEFT],
                                                    calc_from_slot = to_calulcation[loop_calc, NEW_GAME_SLOT_ID],
                                                    precalc_data = pre_aggr_game_status,
                                                    draw_odds_raw_data = to_calulcation[loop_calc, DRAW_ODDS])

    result <- rbind(turns_to_finish_calc, result)


  }
  kesto2 <- difftime(Sys.time(), alku2)
  #kesto2
  print(paste0("kesto single ", kesto2))

}

#  print(difftime(Sys.time(), timenow))

  #we can take max next move, as optimal solution might allow different next moves
  calc_res <- result[, .(NEXT_MOVE = max(NEXT_MOVE)), by = .(TURNS_TO_FINISH, SLOTS_OVER_FINISH , TRACK_LEFT, DECK_LEFT, DRAW_ODDS)]

} else {
  calc_res <- NULL
}

  no_calc <- input_data[IS_FINISHED == 1]
  no_calc[, ':=' (IS_FINISHED = NULL, N = NULL, NEW_GAME_SLOT_ID = NULL, NEXT_MOVE = NA, TURNS_TO_FINISH = 0, SLOTS_OVER_FINISH = NEW_GAME_SLOT_ID - finish_slot, DRAW_ODDS = "", DECK_LEFT = "")]
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

    if (db_handling == "SAVE") {

      join_known <- ADM_OPTIMAL_MOVES[join_res, on = .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS)][is.na(TURNS_TO_FINISH), .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS, TURNS_TO_FINISH = i.TURNS_TO_FINISH,
                                                                                                                    NEXT_MOVE = i.NEXT_MOVE, SLOTS_OVER_FINISH = i.SLOTS_OVER_FINISH)]

    #  join_known[, .N, by = .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS)][N > 1]
       # new_result_row <- join_res[, .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS, TURNS_TO_FINISH, SLOTS_OVER_FINISH,
     #                                NEXT_MOVE)]
      dbWriteTable(con, "ADM_OPTIMAL_MOVES", join_known, append = TRUE, row.names = FALSE)
    #dbIns("ADM_OPTIMAL_MOVES", join_known, con)
    # joinaa <- ADM_OPTIMAL_MOVES[new_result_row, on = .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS)]

    # if (nrow(joinaa) > 0) {

    ADM_OPTIMAL_MOVES <<- rbind(ADM_OPTIMAL_MOVES, join_known)
    }
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
#[, TURNS_TO_FINISH]
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

  final_result_list <- join_res[input_data, on = .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS)]
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
