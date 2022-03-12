calc_combinations_data <- function(con, game_status, deck_status, pre_aggr_game_status_no_list, matr_ijk, reverse_slots_squares, slip_map_matrix,
                                    STG_CYCLER, calc_ttf = 0, case_count = 1000
                                  ) {
#calc ttf takes in current turn of game

#  case_count  <- input_case_count
#pre_aggr_game_status_no_list <- pre_agg_no_list
deck_copied <- copy(deck_status)




  cyclers <- game_status[CYCLER_ID > 0, CYCLER_ID]


 # sscols <- range[, .(TEAM_ID, CYCLER_ID, MOVEMENT)]
  sscols <- deck_status[Zone != "Removed", .N, by = .(CYCLER_ID, MOVEMENT)][CYCLER_ID %in% cyclers][, N := NULL]
 # sscols[, TEAM_ID := 0]

  #sscols <- res_move[sscols_all, on = .(CYCLER_ID, MOVEMENT)][OPTION == TRUE | CYCLER_ID != moving_cycler]

  aggr <- sscols[, .N, by = CYCLER_ID]
  poss_combs <- prod(aggr[, N])
  cyclers <- sscols[, .N, by = CYCLER_ID][, CYCLER_ID]
  tot_data <- data.table(del_me = "")
  for (cyc_loop in cyclers) {
    tot_data <- CJ.dt(tot_data, sscols[CYCLER_ID == cyc_loop, .(CYCLER_ID, MOVEMENT)])

  }
  tot_data[, del_me := NULL]
  appendloop <-  tot_data[, 1:2, with = FALSE]
  setnames(appendloop, colnames(appendloop), c("CYCLER_ID",  "MOVEMENT"))
  appendloop[, case_id := seq_len(.N)]
  tot_data[, (1:2) := NULL]
  if (length(cyclers) > 1) {
    for (appendCOunt in 1:(length(cyclers) - 1)) {

      #tot_data[, del_me := NULL]
      splitcol <- tot_data[, 1:2, with = FALSE]
      setnames(splitcol, colnames(splitcol), c("CYCLER_ID",  "MOVEMENT"))
      splitcol[, case_id := seq_len(.N)]

      appendloop <- rbind(appendloop, splitcol)
      tot_data[, (1:2) := NULL]

    }
  }
  filter_zero_all <- appendloop[order(case_id, CYCLER_ID)]
  cases <- filter_zero_all[, .SD[sample(.N, min(case_count, .N))], by = .(CYCLER_ID, MOVEMENT)][, .N, by = case_id]
  # print(nrow(filter_zero_all))
  filter_zero <- filter_zero_all[case_id %in% cases[, case_id]]
  # print(nrow(filter_zero))



  filter_zero
  #join deck left



  #curr pos
  currpos <- game_status[CYCLER_ID > 0, .(CYCLER_ID, curr_pos = GAME_SLOT_ID, curr_square = SQUARE_ID)]
  #add_drow_odds here

  join_curr_pos <- currpos[filter_zero, on = "CYCLER_ID"]


  #thinking_time <- 2
  setorder(join_curr_pos, case_id, -curr_square, -curr_pos)



  join_curr_pos[, c("cyc_id_copy",
                "MOVE_DIFF",
                "NEW_GAME_SLOT_ID",
                "EXHAUST",
                "NEW_SQUARE") := as.data.table(move_cycler_c(as.matrix(.SD), matr_ijk, reverse_slots_squares, slip_map_matrix)),
            by = .(case_id), .SDcols = c("CYCLER_ID", "MOVEMENT", "curr_pos")]



  new_positions_by_cycler <- join_curr_pos[, .N, by = .(NEW_GAME_SLOT_ID, MOVEMENT, CYCLER_ID)]
  new_positions_by_cycler[, row_id_calc := seq_len(.N)]

  new_positions_by_cycler[, DECK_LEFT := convert_deck_left_to_text(deck_copied,
                                                           CYCLER_ID, MOVEMENT, trunc = TRUE),
                  by = .(row_id_calc)]

  #join team_id
  pos_with_team <- STG_CYCLER[, .(CYCLER_ID, TEAM_ID)][new_positions_by_cycler, on = "CYCLER_ID"]


copy_pre <- copy(pre_aggr_game_status_no_list)
ss_track_left <- copy_pre[, .(NEW_GAME_SLOT_ID = GAME_SLOT_ID, TRACK_LEFT)]
join_track_left <- ss_track_left[pos_with_team, on = .(NEW_GAME_SLOT_ID)]
join_track_left[, N := NULL]
#no draw odds in public information
join_track_left[, DRAW_ODDS := ""]
#join known results

join_track_left[, IS_FINISHED := ifelse(TRACK_LEFT %in% c("", "N"), 1, 0)]

# if (calc_ttf == 0) {

aggr_opt_moves <- ADM_OPTIMAL_MOVES[, .(TRACK_LEFT = TRACK_LEFT[which.min(PRIORITY)],
                                        DECK_LEFT  = DECK_LEFT [which.min(PRIORITY)],
                                        TURNS_TO_FINISH  = TURNS_TO_FINISH [which.min(PRIORITY)],
                                        DRAW_ODDS  = DRAW_ODDS [which.min(PRIORITY)],
                                        SLOTS_OVER_FINISH = SLOTS_OVER_FINISH[which.min(PRIORITY)],
                                        NEXT_MOVE = NEXT_MOVE[which.min(PRIORITY)]),
                                    by = .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS)]

join_known <- aggr_opt_moves[join_track_left, on = .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS)]
join_known[, row_id_calc := NULL]

added_ttf <- add_ttf_multicore(con, join_known[is.na(TURNS_TO_FINISH) & IS_FINISHED == 0], pre_aggr_game_status_no_list)

join_known <- update_dt_values(join_known, added_ttf, c("DRAW_ODDS", "TRACK_LEFT", "DECK_LEFT"),
                 c("TURNS_TO_FINISH", "SLOTS_OVER_FINISH", "NEXT_MOVE"))
finish_slot <- pre_aggr_game_status_no_list[FINISH == 1, GAME_SLOT_ID]
join_known[is.na(TURNS_TO_FINISH) & IS_FINISHED == 1, ':=' (TURNS_TO_FINISH = 0, SLOTS_OVER_FINISH = NEW_GAME_SLOT_ID - finish_slot, NEXT_MOVE = 5)]
# join_known[is.na(TURNS_TO_FINISH), c("TURNS_TO_FINISH", "SLOTS_OVER_FINISH", "NEXT_MOVE")
#            := (finish_turns_db(con, TRACK_LEFT, DECK_LEFT, pre_aggr_game_status_no_list, NEW_GAME_SLOT_ID,
#                                                                                    draw_odds_raw_data = DRAW_ODDS, save_to_DB = TRUE)),
#                   by = .(NEW_GAME_SLOT_ID, TRACK_LEFT, DECK_LEFT, DRAW_ODDS)]
# } else {
#
#   join_known <- join_track_left
#   join_known[, ':=' (TURNS_TO_FINISH = 15 - calc_ttf, SLOTS_OVER_FINISH = 1, NEXT_MOVE = 0)]
#   }
#if (join_known[, min(TURNS_TO_FINISH)] == 1) {browser()}



  join_to_combinations <- join_known[join_curr_pos, on = .(NEW_GAME_SLOT_ID, CYCLER_ID, MOVEMENT)]
 # join_to_combinations <- join_known[jcp, on = .(NEW_GAME_SLOT_ID, CYCLER_ID, MOVEMENT)]
  #join_to_combinations <- join_known[join_curr_pos, on = .(NEW_GAME_SLOT_ID, CYCLER_ID, MOVEMENT), allow.cartesian =TRUE]



  #(#DIST_TO_TEAM = 1 - 1 / (floor(abs(sum(negative_new_game_slot_id)) / 1.1) + 1.5) ^ 2,


  # join_track_left[, DIST_TO_TEAM2 := min(max(dist_to_team_uncapped, 1) - 1, 2), by = .(case_id, TEAM_ID, CYCLER_ID)]



  #FINISH SCORE
  finish_slot <- pre_aggr_game_status_no_list[FINISH == 1, GAME_SLOT_ID]


#first sort fo get correct moves, cyclers order
  setorder(join_to_combinations, case_id, CYCLER_ID)
  join_to_combinations[, ':='  (MOVES = paste0(MOVEMENT, collapse = "_"),
                                CYCLERS = paste0(CYCLER_ID, collapse = "_")) , by = .(case_id, TEAM_ID)]
  #then sorts cyclers by case_id and new_square
  setorder(join_to_combinations, case_id, -NEW_SQUARE)
  #  join_track_left[, ':=' (MY_TEAM_ROW = ifelse(CYCLER_ID %in% smart_cycler_ids, 1,NA))]
  #join_track_left[, ':=' (MY_TEAM_MIN_TTF = min(TURNS_TO_FINISH * MY_TEAM_ROW, na.rm = TRUE)), by = case_id]
  # join_track_left[, ':=' (RELEVANT_OPPONENT = MY_TEAM_MIN_TTF >= (TURNS_TO_FINISH - 1)), by = case_id]
  join_to_combinations[, ':=' (CYCLER_MEAN_TTF = mean(TURNS_TO_FINISH, na.rm = TRUE)), by = .(CYCLER_ID)]
  join_to_combinations[, ':=' (CYCLER_MEAN_SOF = mean(SLOTS_OVER_FINISH, na.rm = TRUE)), by = .(CYCLER_ID)]



  join_to_combinations[, ':=' (SLOTS_PROGRESSED = NEW_GAME_SLOT_ID - curr_pos)]
  join_to_combinations[, ':=' (
    MOVE_ORDER = seq_len(.N),
    MOVE_DIFF_RELATIVE = MOVE_DIFF - (sum(MOVE_DIFF) - MOVE_DIFF) / (.N - 1)
  ), by = case_id]
  join_to_combinations[, ':=' (MOVE_DIFF_RELATIVE = ifelse(is.na(MOVE_DIFF_RELATIVE), 1, MOVE_DIFF_RELATIVE))]
  join_to_combinations[, ':=' (FINISH_ESTIMATE = MOVE_ORDER / 100 + TURNS_TO_FINISH - pmin(SLOTS_OVER_FINISH, 4) / 5)]
  join_to_combinations[, ':=' (FINISH_ESTIMATE_MEAN = mean(FINISH_ESTIMATE)), by = .(CYCLER_ID)]
  join_to_combinations[, OVER_FINISH := pmax(NEW_GAME_SLOT_ID - finish_slot, 0) + ((TURNS_TO_FINISH == 0 * (4 - pmin(MOVE_ORDER , 4))) * 2)]
  result <- join_to_combinations
  return(result)

}
