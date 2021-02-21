
#prepare most likely needed optimal moves
cyclers_turns_MOVEMEMENT_combs <- function(con, ADM_OPTIMAL_MOVES, game_status, deck_status, pre_aggr_game_status, calc_ttf = TRUE, calc_draw_odds = FALSE,
                                           subset_team = NULL) {


  used_game_status <- copy(game_status)
  deck_copied <- copy(deck_status)
  curr_posits <- used_game_status[CYCLER_ID > 0, .(CYCLER_ID, curr_pos = GAME_SLOT_ID, MAXIMUM_MOVEMENT, MINIMUM_MOVEMENT)]


  #we dont have data for this turn, lets calc it
  not_played <- deck_copied[Zone != "Removed"]

  #this can be improved to use the known information when exhaust has been added
  options <- not_played[, .N, by = .(MOVEMENT, CYCLER_ID)][order(CYCLER_ID)]
  #options[, turns_to_finish := 100]

  #join current position
  #join_curr <- curr_posits[, on = "CYCLER_ID"]
  join_curr_all <- options[curr_posits, on = "CYCLER_ID"]

  join_curr <- join_curr_all


  join_curr[, new_slot_after_moving := move_cycler(used_game_status, CYCLER_ID, MOVEMENT,
                                                   slipstream = FALSE,
                                                   ignore_block = TRUE,
                                                   return_numeric_position = TRUE), by = .(CYCLER_ID, MOVEMENT)]

  if (calc_draw_odds == FALSE) {
  join_curr[, DRAW_ODDS := ""]
  } else {

    join_curr[, DRAW_ODDS := calculate_draw_distribution_by_turn(CYCLER_ID, deck_status, how_many_cards = 4, db_res = TRUE), by = .(CYCLER_ID)]
  }
  join_curr[, row_id_calc := seq_len(.N)]
  ss_cols_track_left <- pre_aggr_game_status$aggr_to_slots[, .(GAME_SLOT_ID, TRACK_LEFT)]
  finish_slot <- pre_aggr_game_status$aggr_to_slots[FINISH == 1, GAME_SLOT_ID]

  join_track_left <- ss_cols_track_left[join_curr, on = .(GAME_SLOT_ID = new_slot_after_moving)]
  if (!is.null(subset_team)) {
    join_track_left <- join_track_left[TEAM_ID %in% subset_team]
  }

  setnames(join_track_left, "GAME_SLOT_ID", "new_slot_after_moving")

  join_track_left[, DECK_LEFT := convert_deck_left_to_text(play_card(CYCLER_ID,
                                                                     card_id = NULL,
                                                                     deck_copied,
                                                                     game_id = 0,
                                                                     turn_id = 0,
                                                                     con = NULL,
                                                                     card_row_id = FALSE,
                                                                     MOVEMENT_PLAYED = MOVEMENT,
                                                                     force = TRUE,
                                                                     copy = TRUE),
                                                           CYCLER_ID),
                  by = .(row_id_calc)]
  if (calc_ttf == TRUE) {
  #check if we already have results
  joined <- ADM_OPTIMAL_MOVES[join_track_left, on = .(DECK_LEFT, TRACK_LEFT, DRAW_ODDS)]
  # joined[is.na(TURNS_TO_FINISH), TURNS_TO_FINISH := as.double(finish_turns_db(con, track_left_input = TRACK_LEFT,
  #                                                                   cycler_deck_status = DECK_LEFT,
  #                                                                   pre_aggr_game_status,
  #                                                                   cycler_at_slot = new_slot_after_moving,
  #                                                                   draw_odds_raw_data = DRAW_ODDS, save_to_DB = TRUE)), by = .(TRACK_LEFT,
  #                                                                                                                               DECK_LEFT,
  #                                                                                                                               new_slot_after_moving)]

    joined[is.na(TURNS_TO_FINISH), TURNS_TO_FINISH := as.double(finish_turns_db(con, track_left_input = TRACK_LEFT,
                                                                                cycler_deck_status = DECK_LEFT,
                                                                                pre_aggr_game_status,
                                                                                cycler_at_slot = new_slot_after_moving,
                                                                                draw_odds_raw_data = DRAW_ODDS, save_to_DB = TRUE)), by = .(row_id_calc)]
  } else {
    joined <- join_track_left
    joined[, TURNS_TO_FINISH := 10]
  }
  #   for (opt_loop in 1:nrow(join_curr)) {
  #     loop_cycler <- join_curr[opt_loop, CYCLER_ID]
  #     cycler_deck_updated <- deck_copied[CYCLER_ID == join_curr[opt_loop, CYCLER_ID]]
  #     played_card <- join_curr[opt_loop, MOVEMENT]
  #     min_row_id_played <- cycler_deck_updated[Zone != "Removed", min(row_id)]
  #     cycler_deck_updated[row_id == min_row_id_played, Zone := "Removed"]
  #
  #
  #     slot <- join_curr[opt_loop, new_slot_after_moving]
  #
  #     draw_odds_input <- ""
  #     res_temp <- finish_turns_db(con, ADM_OPTIMAL_MOVES, used_game_status, cycler_deck_updated, pre_aggr_game_status, slot, draw_odds_input)
  #     ADM_OPTIMAL_MOVES <<- res_temp$new_ADM_OPT
  #
  #     join_curr[opt_loop, turns_to_finish := res_temp$turns_to_finish]
  # }

    joined[, actual_movement := new_slot_after_moving - curr_pos]

    res_list <- NULL
    res_list$ctM_data <- joined
    return(res_list)

}
