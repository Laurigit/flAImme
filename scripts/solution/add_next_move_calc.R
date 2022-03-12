add_next_move_calc <- function(hidden_info_data, matr_ijk, reverse_slots_squares, slip_map_matrix, pre_aggr_game_status_no_list,
                               STG_CYCLER, my_team) {
  #cp_data <-hidden_information_output

  cp_data <- copy(hidden_info_data)
  #this is to purkka finished cyclers
  cp_data[OVER_FINISH > 0, ':=' (NEXT_MOVE = 1, NEW_GAME_SLOT_ID = CYCLER_ID + 3)]

  cp_data[, c("NEXT_cyc_id_copy",
                    "NEXT_MOVE_DIFF",
                    "NEXT_NEW_GAME_SLOT_ID",
                    "NEXT_EXHAUST",
                    "NEXT_NEW_SQUARE") := as.data.table(move_cycler_c(as.matrix(.SD), matr_ijk, reverse_slots_squares, slip_map_matrix)),
                by = .(case_id), .SDcols = c("CYCLER_ID", "NEXT_MOVE", "NEW_GAME_SLOT_ID")]

#remove already played card
cp_data[, rowid := seq_len(.N)]
cp_data[, DECK_LEFT := sub(MOVEMENT, "", DECK_LEFT), by = rowid]
#add exhaust to deck
cp_data[, DECK_LEFT := ifelse(EXHAUST == 1, paste0(DECK_LEFT, "2"), DECK_LEFT)]



new_positions_by_cycler <- cp_data[NEXT_MOVE_DIFF != 0 & TEAM_ID == my_team, .N, by = .(NEXT_NEW_GAME_SLOT_ID, MOVEMENT, CYCLER_ID, DECK_LEFT)]



#join team_id

pos_with_team <- STG_CYCLER[, .(CYCLER_ID, TEAM_ID)][new_positions_by_cycler, on = "CYCLER_ID"]
  copy_pre <- copy(pre_aggr_game_status_no_list)
  ss_track_left <- copy_pre[, .(NEXT_NEW_GAME_SLOT_ID = GAME_SLOT_ID, TRACK_LEFT)]
  join_track_left <- ss_track_left[pos_with_team, on = .(NEXT_NEW_GAME_SLOT_ID)]

  join_track_left[, DRAW_ODDS := ""]
  #join known results
  aggr_opt_moves <- ADM_OPTIMAL_MOVES[, .(TRACK_LEFT = TRACK_LEFT[which.min(PRIORITY)],
                                          DECK_LEFT  = DECK_LEFT [which.min(PRIORITY)],
                                          TURNS_TO_FINISH  = TURNS_TO_FINISH [which.min(PRIORITY)],
                                          DRAW_ODDS  = DRAW_ODDS [which.min(PRIORITY)],
                                          SLOTS_OVER_FINISH = SLOTS_OVER_FINISH[which.min(PRIORITY)],
                                          NEXT_MOVE = NEXT_MOVE[which.min(PRIORITY)])]

    join_known <- aggr_opt_moves[join_track_left, on = .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS)]
   # join_known[, row_id_calc := NULL]
    join_known[, IS_FINISHED := ifelse(TRACK_LEFT %in% c("", "N"), 1, 0)]
    added_ttf <- add_ttf_multicore(con, join_known[is.na(TURNS_TO_FINISH) & IS_FINISHED == 0], pre_aggr_game_status_no_list, "NEXT_NEW_GAME_SLOT_ID")

    #browser()

    join_known <- update_dt_values(join_known, added_ttf, c("DRAW_ODDS", "TRACK_LEFT", "DECK_LEFT"),
                                         c("TURNS_TO_FINISH", "SLOTS_OVER_FINISH", "NEXT_MOVE"))
    finish_slot <- pre_aggr_game_status_no_list[FINISH == 1, GAME_SLOT_ID]
    join_known[is.na(TURNS_TO_FINISH) & IS_FINISHED == 1, ':=' (TURNS_TO_FINISH = 0, SLOTS_OVER_FINISH = NEXT_NEW_GAME_SLOT_ID  - finish_slot, NEXT_MOVE = 5)]
    # join_known[is.na(TURNS_TO_FINISH), c("TURNS_TO_FINISH", "SLOTS_OVER_FINISH", "NEXT_MOVE") := (finish_turns_db(con, TRACK_LEFT, DECK_LEFT, pre_aggr_game_status_no_list, NEXT_NEW_GAME_SLOT_ID,
    #                                                                                                               draw_odds_raw_data = DRAW_ODDS, save_to_DB = TRUE)),
    #            by = .(NEXT_NEW_GAME_SLOT_ID, TRACK_LEFT, DECK_LEFT, DRAW_ODDS)]



    ss_res <- join_known[, .(DECK_LEFT, TURNS_TO_FINISH_NEW = TURNS_TO_FINISH, SLOTS_OVER_FINISH_NEW = SLOTS_OVER_FINISH,
                             NEXT_NEW_GAME_SLOT_ID, CYCLER_ID, TRACK_LEFT)]
join_res <- ss_res[cp_data, on = .(DECK_LEFT, NEXT_NEW_GAME_SLOT_ID, CYCLER_ID, TRACK_LEFT)]
finish_slot <- copy_pre[FINISH == 1, GAME_SLOT_ID]
join_res[is.na(SLOTS_OVER_FINISH_NEW), SLOTS_OVER_FINISH_NEW := SLOTS_OVER_FINISH]
join_res[is.na(TURNS_TO_FINISH_NEW), TURNS_TO_FINISH_NEW := TURNS_TO_FINISH - 1 ]

setorder(join_res, case_id, -NEXT_NEW_SQUARE)

join_res[, ':=' (NEW_MOVE_ORDER = seq_len(.N)), by = case_id]



return(join_res)
}
