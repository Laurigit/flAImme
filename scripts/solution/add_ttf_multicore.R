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


  #first clear cases where we cant reach finish line without extra 2s
  result_simple <- NULL
  turns_to_finish_calc <- NULL

#to_calulcation <- rbind(to_calulcation, data.table(DECK_LEFT = "9322", NEW_GAME_SLOT_ID = "65",
#                                                   DRAW_ODDS = "11;23", TRACK_LEFT = "AANNNNNNNN",
#                                                   IS_FINISHED = 0, N = 1))
  for (loop_calc in 1:nrow(to_calulcation)) {


    turns_to_finish_calc <- simple_optimal_moves(cycler_deck_status = to_calulcation[loop_calc, DECK_LEFT],
                                                    calc_from_slot = to_calulcation[loop_calc, NEW_GAME_SLOT_ID],
                                                    precalc_data = pre_aggr_game_status,
                                                    draw_odds_raw_data = to_calulcation[loop_calc, DRAW_ODDS])

    result_simple <- rbind(turns_to_finish_calc, result_simple)


  }

  if (!is.null(result_simple)) {
  solved_cases <- result_simple[, .N, by = .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS, TURNS_TO_FINISH)][, N := NULL]

  unsolved <- solved_cases[to_calulcation, on = .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS)][is.na(TURNS_TO_FINISH)]
  unsolved[, TURNS_TO_FINISH := NULL]
  } else {
    unsolved <- to_calulcation
  }


if (nrow(unsolved) > 0) {
  print(nrow(unsolved))
  len <- unsolved[1, nchar(TRACK_LEFT)]
#print(len)
tulo <- len * nrow(unsolved)
  if ((tulo > 1000) & global_dont_multicore == FALSE) {
  klusteri <- (parallel::makeCluster(global_cores))
  registerDoParallel(klusteri)

  #foreach::getDoParRegistered()

#if (global_dont_multicore == FALSE) {
#print("start multicore")
  alku <- Sys.time()
  result <- foreach(i = 1:nrow(unsolved), .combine = 'rbind',
                    .packages = c("data.table", "stringr", "ompr", "ROI", "ROI.plugin.symphony",
                             "ompr.roi", "optiRum")) %dopar% {
                                source("./scripts/solution/optimal_moves_to_finish.R")
                                source("./scripts/solution/boundi_filtteri.R")
                               source("./scripts/solution/clean_model_res.R")

                                turns_to_finish_calc <- optimal_moves_to_finish(cycler_deck_status = unsolved[i, DECK_LEFT],
                                                                                calc_from_slot = unsolved[i, NEW_GAME_SLOT_ID],
                                                                                precalc_data = pre_aggr_game_status,
                                                                                draw_odds_raw_data = unsolved[i, DRAW_ODDS])

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

  for (loop_calc in 1:nrow(unsolved)) {

    alku3 <- Sys.time()
  #  print(loop_calc)
#print(loop_calc)
#if(loop_calc == 87) {browser()}
    turns_to_finish_calc <- optimal_moves_to_finish(cycler_deck_status = unsolved[loop_calc, DECK_LEFT],
                                                    calc_from_slot = unsolved[loop_calc, NEW_GAME_SLOT_ID],
                                                    precalc_data = pre_aggr_game_status,
                                                    draw_odds_raw_data = unsolved[loop_calc, DRAW_ODDS])


    result <- rbind(turns_to_finish_calc, result)

    kesto3 <- difftime(Sys.time(), alku3)
  #  print(paste0("kesto single loop ", kesto3))
  }
  kesto2 <- difftime(Sys.time(), alku2)
  #kesto2
  print(paste0("kesto single ", kesto2))

}

#  print(difftime(Sys.time(), timenow))

#append simple and unsolved

combined_solved <- rbind(result_simple, result)

  #we can take max next move, as optimal solution might allow different next moves

combined_solved[, row_id := seq_len(.N)]
  calc_res <- combined_solved[, .(NEXT_MOVE = NEXT_MOVE[which.min(row_id)],
                         FOLLOWING_MOVE = FOLLOWING_MOVE[which.min(row_id)],
                         SOLUTION = SOLUTION[which.min(row_id)],
                         PARENT_ID = PARENT_ID[which.min(row_id)]), by = .(TURNS_TO_FINISH, SLOTS_OVER_FINISH,
                                                                           TRACK_LEFT, DECK_LEFT, DRAW_ODDS,
                                                                           PRIORITY, SAVE)]

} else {
  if (nrow(result_simple)> 0) {


    #we can take max next move, as optimal solution might allow different next moves

    result_simple[, row_id := seq_len(.N)]
    calc_res <- result_simple[, .(NEXT_MOVE = NEXT_MOVE[which.min(row_id)],
                                    FOLLOWING_MOVE = FOLLOWING_MOVE[which.min(row_id)],
                                    SOLUTION = SOLUTION[which.min(row_id)],
                                    PARENT_ID = PARENT_ID[which.min(row_id)]), by = .(TURNS_TO_FINISH, SLOTS_OVER_FINISH,
                                                                                      TRACK_LEFT, DECK_LEFT, DRAW_ODDS,
                                                                                      PRIORITY, SAVE)]
  } else {
  calc_res <- NULL
  }
}
  input_data[, SAVE := FALSE]
  no_calc <- input_data[IS_FINISHED == 1]
  no_calc[, ':=' (IS_FINISHED = NULL, N = NULL, NEW_GAME_SLOT_ID = NULL, NEXT_MOVE = NA, PARENT_ID = paste0(TRACK_LEFT, "_", DECK_LEFT, "_", DRAW_ODDS),
                  SOLUTION = "", FOLLOWING_MOVE = NA, TURNS_TO_FINISH = 0, SLOTS_OVER_FINISH = NEW_GAME_SLOT_ID - finish_slot, DRAW_ODDS = "", DECK_LEFT = "", PRIORITY = 3)]

  join_res <- rbind(calc_res, no_calc)
 # join_res[, NEW_GAME_SLOT_ID := NULL]

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
      # join_known_all <- ADM_OPTIMAL_MOVES[join_res, on = .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS, PRIORITY)]
      # join_known <- join_known_all[is.na(TURNS_TO_FINISH), .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS, TURNS_TO_FINISH = i.TURNS_TO_FINISH,
      #                                                         NEXT_MOVE = i.NEXT_MOVE, SLOTS_OVER_FINISH = i.SLOTS_OVER_FINISH,
      #                                                         PARENT_ID = i.PARENT_ID, PRIORITY,
      #                                                        FOLLOWING_MOVE = i.FOLLOWING_MOVE,
      #                                                        SOLUTION = i.SOLUTION)]


           join_known <- join_res[SAVE == TRUE]
           join_known[, SAVE := NULL]

           join_known[DRAW_ODDS != "", next_move_draw_odds_step_1 := list(list(unique(as.numeric(unlist(str_split(str_split(str_split(DRAW_ODDS, ";")[[1]][2], "\\.")[[1]][1], "")))))), by = .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS)]
           join_known[DRAW_ODDS != "", next_move_draw_odds_step_2 :=  list(list(unique(as.numeric(unlist(str_split(str_split(str_split(DRAW_ODDS, ";")[[1]][2], "\\.")[[1]][2], "")))))), by = .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS)]
           join_known[, max_dod_turns := max(as.numeric(unlist(str_split(str_split(DRAW_ODDS, ";")[[1]][1],"")))), by = .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS)]
           join_known[max_dod_turns >= 1, IS_VALID_1 := ifelse(NEXT_MOVE %in% unlist(next_move_draw_odds_step_1), TRUE, FALSE), by = .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS)]
           join_known[max_dod_turns >=  2, IS_VALID_2 := ifelse(is.na(FOLLOWING_MOVE) | FOLLOWING_MOVE %in% unlist(next_move_draw_odds_step_2), TRUE, FALSE), by = .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS)]
           invalid_rows <- join_known[IS_VALID_1 == FALSE | IS_VALID_2 == FALSE]

           if (nrow(invalid_rows) > 0) {
             browser()
           }
           join_known[, ':=' (max_dod_turns := NULL,
                              next_move_draw_odds_step_1 = NULL,
                              next_move_draw_odds_step_2 = NULL,
                              IS_VALID_1 = NULL,
                              IS_VALID_2 = NULL)]
           #join_known[116, next_move_draw_odds_step_1[[1]]]
            max_ttf <- join_known[, max(TURNS_TO_FINISH)]
           if (max_ttf > 22) {browser()}
    #  join_known[, .N, by = .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS)][N > 1]
       # new_result_row <- join_res[, .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS, TURNS_TO_FINISH, SLOTS_OVER_FINISH,
     #                                NEXT_MOVE)]

      dbWriteTable(con, "ADM_OPTIMAL_MOVES", join_known, append = TRUE, row.names = FALSE)
    #dbIns("ADM_OPTIMAL_MOVES", join_known, con)
    # joinaa <- ADM_OPTIMAL_MOVES[new_result_row, on = .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS)]

    # if (nrow(joinaa) > 0) {
     # join_known[, PARENT_ID := NULL]
   # ADM_OPTIMAL_MOVES <<- rbind(ADM_OPTIMAL_MOVES, join_known)
    }
    #  print(paste0("in function ", nrow(ADM_OPTIMAL_MOVES)))
    #  }

    FALSE
  }, error = function(e) {

    warning("tried to insert duplicate to ADM_OPTIMAL")

  })


  aggr_res_over_prio <- join_res[, .(NEXT_MOVE = NEXT_MOVE[which.min(PRIORITY)],
                                     FOLLOWING_MOVE = FOLLOWING_MOVE[which.min(PRIORITY)],

                                     TURNS_TO_FINISH = TURNS_TO_FINISH[which.min(TURNS_TO_FINISH)],
                                     SLOTS_OVER_FINISH = SLOTS_OVER_FINISH[which.min(SLOTS_OVER_FINISH)]), by = .(
                                                                                       TRACK_LEFT, DECK_LEFT, DRAW_ODDS
                                                                                       )]

  ss_prev <- ADM_OPTIMAL_MOVES_AGGR[, .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS, MISSING = TRUE)]
  check_existing <- ss_prev[aggr_res_over_prio, on = .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS)][is.na(MISSING)]
  check_existing[, MISSING := NULL]

  ADM_OPTIMAL_MOVES_AGGR <<- rbind(ADM_OPTIMAL_MOVES_AGGR, check_existing)


  final_result_list <- aggr_res_over_prio[input_data, on = .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS)]
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
