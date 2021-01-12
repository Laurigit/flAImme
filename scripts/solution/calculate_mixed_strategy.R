# track <- 2
# cyclers <- c(4,1,3,6,2,5)
# game_status <- start_game(cyclers,track, STG_TRACK_PIECE, STG_TRACK)
# ijk <- ijk_map(track, STG_TRACK_PIECE, STG_TRACK)

#calculate_mixed_strategy
calculate_mixed_strategy <- function(game_status, deck_status, ijk, ADM_AI_CONF, ADM_OPTIMAL_MOVES, ctM_data, STG_TEAM) {


  slots_squares <- as.matrix(game_status[, .(SQUARE_ID, GAME_SLOT_ID)])
  reverse_slots_squares <- slots_squares[nrow(slots_squares):1,]
  #


    range <- suppressWarnings(calc_move_range(game_status, deck_status, ctM_data, STG_CYCLER))

    cyclers <- game_status[CYCLER_ID > 0, CYCLER_ID]

    sscols <- range[, .(TEAM_ID, CYCLER_ID, MOVEMENT)]
    #eventhough I know what cards I have, others dont know it.
    #sscols <- res_move[sscols_all, on = .(CYCLER_ID, MOVEMENT)][OPTION == TRUE | CYCLER_ID != moving_cycler]

    aggr <- sscols[, .N, by = CYCLER_ID]
    poss_combs <- prod(aggr[, N])
    cyclers <- sscols[, .N, by = CYCLER_ID][, CYCLER_ID]
    tot_data <- data.table(del_me = "")
    for (cyc_loop in cyclers) {
      tot_data <- CJ.dt(tot_data, sscols[CYCLER_ID == cyc_loop, .(CYCLER_ID, odds = 0,  MOVEMENT)])

    }
    tot_data[, del_me := NULL]
    appendloop <-  tot_data[, 1:3, with = FALSE]
    setnames(appendloop, colnames(appendloop), c("CYCLER_ID", "odds", "MOVEMENT"))
    appendloop[, case_id := seq_len(.N)]
    tot_data[, (1:3) := NULL]
    for (appendCOunt in 1:(length(cyclers) - 1)) {

      #tot_data[, del_me := NULL]
      splitcol <- tot_data[, 1:3, with = FALSE]
      setnames(splitcol, colnames(splitcol), c("CYCLER_ID", "odds", "MOVEMENT"))
      splitcol[, case_id := seq_len(.N)]

      appendloop <- rbind(appendloop, splitcol)
      tot_data[, (1:3) := NULL]
    }
    filter_zero <- appendloop[order(case_id, CYCLER_ID)]


    #join deck left
    cycler_movement <- range[, .(CYCLER_ID, MOVEMENT, DECK_LEFT, DRAW_ODDS)]
    join_deck_left <- cycler_movement[filter_zero, on = .(CYCLER_ID, MOVEMENT)]



    #curr pos
    currpos <- game_status[CYCLER_ID > 0, .(CYCLER_ID, GAME_SLOT_ID, SQUARE_ID)]
    #add_drow_odds here

    #calculate draw odds for smart cyclers
    #jionteam
    currpos_team <- STG_CYCLER[currpos, on = "CYCLER_ID"][, .(CYCLER_ID, TEAM_ID, curr_pos = GAME_SLOT_ID, curr_square = SQUARE_ID)]
    #join tosimul
    join_info_all <- currpos_team[join_deck_left, on = "CYCLER_ID"]


    #aggr_odds <- join_info[case_id == 1, .(case_odds = prod(odds), cyc_vec = list(phaseless_simulation(game_status, 5, 6, 3, STG_CYCLER, .SD, TRUE))), by = case_id, .SDcols = c("MOVEMENT", "CYCLER_ID", "curr_pos", "TEAM_ID")]
    join_info_all[, case_odds := prod(odds), by = case_id]
    join_info_all[, case_odds_ranking_total := frank(-case_odds, ties.method = c("min"))]
    join_info <- join_info_all[order(-case_odds)]

    #join_info[, cycler_odds_rank := frank(-case_odds), by = .(CYCLER_ID, MOVEMENT)]
    #case_odds < 30

    max_new_cyc_pos <- join_info[, max(curr_pos, na.rm = TRUE)]


      #thinking_time <- 2
      setorder(join_info, case_id, -curr_square)

      matr_ijk <- as.matrix(ijk)
      join_info[, c("cyc_id_copy","MOVE_DIFF", "NEW_GAME_SLOT_ID", "EXHAUST", "NEW_SQUARE") := as.data.table(move_cycler_c(as.matrix(.SD), matr_ijk, reverse_slots_squares)),
                by = .(case_id), .SDcols = c("CYCLER_ID", "MOVEMENT", "curr_pos")]

      track_ss <- pre_track$aggr_to_slots[, .(NEW_GAME_SLOT_ID = GAME_SLOT_ID, TRACK_LEFT)]
      join_track_left <- track_ss[join_info, on = .(NEW_GAME_SLOT_ID)]

      join_track_left[!is.na(TRACK_LEFT), TURNS_TO_FINISH := as.double(finish_turns_db(con, TRACK_LEFT, DECK_LEFT, pre_track, NEW_GAME_SLOT_ID,
                                                                                       draw_odds_raw_data = DRAW_ODDS, save_to_DB = TRUE)),
                      by = .(NEW_GAME_SLOT_ID, TRACK_LEFT, DECK_LEFT, DRAW_ODDS)]

      #scoora exhaust, move order, ttf, move_diff, squares over finish, ero tiimikaveriin
      join_track_left[, DIST_TO_TEAM := min(max(as.integer(dist(NEW_GAME_SLOT_ID)), 1) - 1, 2), by = .(case_id, TEAM_ID)]

      #FINISH SCORE
      finish_slot <- pre_track$aggr_to_slots[FINISH == 1, GAME_SLOT_ID]
      join_track_left[, OVER_FINISH := max(NEW_GAME_SLOT_ID - finish_slot, 0)]
      join_track_left[, TTF_RELATIVE := min(TURNS_TO_FINISH) - TURNS_TO_FINISH, by = case_id]
      join_track_left[, MOVE_ORDER := frank(-NEW_SQUARE), by = case_id]
      join_track_left[, MOVE_DIFF_RELATIVE := MOVE_DIFF - (sum(MOVE_DIFF) - MOVE_DIFF) / (.N - 1), by = case_id]
      join_track_left[, ':=' (MOVE_DIFF_SCORE = MOVE_DIFF * 1,
                              EXHAUST_SCORE = EXHAUST * -1,
                              TTF_SCORE = TTF_RELATIVE * 2,
                              CYC_DIST_SCORE = DIST_TO_TEAM * - 0.1,
                              MOVE_ORDER_SCORE = MOVE_ORDER * 0.1,
                              OVER_FINISH_SCORE = OVER_FINISH * 100
      )]

      join_track_left[, TOT_SCORE := (MOVE_DIFF_SCORE +
                                        EXHAUST_SCORE +
                                        TTF_SCORE +
                                        CYC_DIST_SCORE +
                                        MOVE_ORDER_SCORE +
                                        OVER_FINISH_SCORE)]
      join_track_left[, all_moves := list((list(MOVEMENT))), by = case_id]
      join_track_left[,  opponent_moves := create_other_moves(all_moves, CYCLER_ID), by = .(case_id, TEAM_ID)]
      join_track_left[, ':=' (ORIG_DECK_LEFT = paste0(MOVEMENT, DECK_LEFT))]
      join_track_left[, ODDS_OF_DRAWING_MOVEMENT := calculate_odds_of_drawing_card(ORIG_DECK_LEFT, MOVEMENT)]
      join_track_left[, MIX_STRATEGY_CAP := prod(ODDS_OF_DRAWING_MOVEMENT), by = .(case_id, TEAM_ID)]
      join_track_left[, ':=' (PROB_PRODUCT = 0.1, OTHER_ACTION_ODDS = 0.5,
                              ODDS_OF_DRAWING_MOVEMENT = NULL,
                              ORIG_DECK_LEFT = NULL)]
      #ODDS_OF_DRAWING_CASE_MOVEMENTS_BY_TEAM = capped mixed strategy weight

      join_track_left[, ':=' (CYCLERS = paste0(CYCLER_ID, collapse = "_"),
                              MOVES = paste0(MOVEMENT, collapse = "_")), by = .(case_id, TEAM_ID)]
      #calculate new odds based on expect moves of others




      check_res <- join_track_left[, .(CYCLERS, TEAM_SCORE = sum(TOT_SCORE), MOVES), by = .(TEAM_ID, case_id, opponent_moves, PROB_PRODUCT, OTHER_ACTION_ODDS, MIX_STRATEGY_CAP)]

      check_res2 <- check_res[, .(EV = sum(TEAM_SCORE * OTHER_ACTION_ODDS) / sum(OTHER_ACTION_ODDS)), by = .(CYCLERS, MOVES, TEAM_ID, MIX_STRATEGY_CAP)][order(-EV)]

      iteration_limit <- 50
      var_threshold <- 0.00001
      input_gamma <- 0.15
      gamma_addition <- 0.05
      prev_result <- calculated_mix_strategy_inner(check_res2, input_gamma)
      #at least second loop
      continue <- 1
      iter_loop_count <- 0
      while (continue == 1) {
        iter_loop_count <- iter_loop_count + 1

        total_gamma <- input_gamma + floor(iter_loop_count / 10) * gamma_addition
      new_result <- calculated_mix_strategy_inner(prev_result, total_gamma)

      joinaa <- new_result[prev_result, on = .(CYCLERS, MOVES, TEAM_ID)]
      joinaa[, prob_diff := PROB_PRODUCT - i.PROB_PRODUCT]
      varia <-  joinaa[, var(prob_diff)]
      prev_result <- new_result

      if (varia < var_threshold | iter_loop_count >= iteration_limit) {
        continue <- 0
      }
      }
     # new_result[order(- EV)][1:30]





      return(new_result)




}
