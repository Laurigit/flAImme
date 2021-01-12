#TÄTÄ ETSIT. JÄIT SIIHEN, ETTÄ C:llä simuloidaan positiot. siitä pitäis joinaa TTF ja scooraa.
con <- connDB(con, dbname_input = "flaimme")
rm("ADM_OPTIMAL_MOVES")
required_data(c("STG_TRACK", "STG_TRACK_PIECE", "ADM_CYCLER_DECK", "ADM_OPTIMAL_MOVES"))
required_data("ADM_AI_CONF")
track <- 2
cyclers <- c(1,2,3,4,5,6)
game_status <- start_game(cyclers,track, STG_TRACK_PIECE, STG_TRACK)
#game_status <- set_cycler_position(1, 64, 1,  game_status)
deck_status <- create_decks(cyclers, ADM_CYCLER_DECK)
zoom(game_status)


ijk <- ijk_map(track, STG_TRACK_PIECE, STG_TRACK)

#played_cards <- data.table(cards = c(4, 4, 5, 5))

slots_squares <- as.matrix(game_status[, .(SQUARE_ID, GAME_SLOT_ID)])
reverse_slots_squares <- slots_squares[nrow(slots_squares):1,]
#



#everybody draws
for(loop in cyclers) {
  deck_status <- draw_cards(loop, deck_status)
  # print(list(deck_status[CYCLER_ID == loop & Zone == "Hand", CARD_ID]))

}


pre_track <- precalc_track(game_status)
ctM_res <- cyclers_turns_MOVEMEMENT_combs(con, ADM_OPTIMAL_MOVES, game_status, deck_status, pre_track)
ctM_data <- ctM_res$ctM_data
smart_cyclers <- c(5, 6)
moving_cycler <- 5

range <- calc_move_range(game_status, deck_status, ctM_data, STG_TEAM)
team_id <- 3
phase <- 1

card_options_in_hand <- smart_cards_options(deck_status[CYCLER_ID == moving_cycler & Zone == "Hand", unique(MOVEMENT)], pre_track, moving_cycler)
if (length(card_options_in_hand) == 1) {
  res_move <- data.table(MOVEMENT = card_options_in_hand, CYCLER_ID = moving_cycler, OPTION = TRUE)
} else {

  range <- suppressWarnings(calc_move_range(game_status, deck_status, ctM_data, STG_CYCLER))
  if (phase == 2) {
    phase_two_cyclers<-  deck_status[Zone == "Hand", .N, by = CYCLER_ID][, CYCLER_ID]
    p2_score_outout <- phase2_slot_score(game_status, phase_two_cyclers, deck_status, range_joined_team, pre_aggr_game_status)
    range <- suppressWarnings(calc_move_range_phase_2(game_status, deck_status, ctM_data, p2_score_outout, STG_CYCLER))
  }



  cyclers <- game_status[CYCLER_ID > 0, CYCLER_ID]

  #pre_track <- precalc_track(game_status)
  #ctM_res <- cyclers_turns_MOVEMEMENT_combs(con, ADM_OPTIMAL_MOVES, game_status, deck_status, pre_track)


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


  #CALC DRAW ODDS HERE AS IT DEPENDS ON PLAYED CARD! but it only calculated later as opponents donw know my odds
  # range[, DRAW_ODDS :=  ifelse(CYCLER_ID %in% smart_cyclers, (calculate_draw_distribution_by_turn(CYCLER_ID,
  #                                                                                                 play_card(CYCLER_ID,
  #                                                                                                           card_id = NULL,
  #                                                                                                           deck_status,
  #                                                                                                           game_id = 0,
  #                                                                                                           turn_id = 0,
  #                                                                                                           con = NULL,
  #                                                                                                           card_row_id = NULL,
  #                                                                                                           MOVEMENT_PLAYED = MOVEMENT,
  #                                                                                                           copy = TRUE),
  #                                                                                                 4, db_res = TRUE)),
  #                              ""), by = .(CYCLER_ID, MOVEMENT)]

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
  if (phase == 1) {

      #thinking_time <- 2
      setorder(join_info, case_id, -curr_square)
      reverse_slots_squares

      matr_ijk <- as.matrix(ijk)
      join_info[, c("cyc_id_copy","MOVE_DIFF", "NEW_GAME_SLOT_ID", "EXHAUST", "NEW_SQUARE") := as.data.table(move_cycler_c(as.matrix(.SD), matr_ijk, reverse_slots_squares)),
                by = .(case_id), .SDcols = c("CYCLER_ID", "MOVEMENT", "curr_pos")]

      track_ss <- pre_track$aggr_to_slots[, .(NEW_GAME_SLOT_ID = GAME_SLOT_ID, TRACK_LEFT)]
      join_track_left <- track_ss[join_info, on = .(NEW_GAME_SLOT_ID)]
      required_data("ADM_OPTIMAL_MOVES")
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
      join_track_left[, ':=' (PROB_PRODUCT = 0.1, OTHER_ACTION_ODDS = 0.5,
                              ORIG_DECK_LEFT = paste0(MOVEMENT, DECK_LEFT))]
      join_track_left[, ODDS_OF_DRAWING_MOVEMENT := calculate_odds_of_drawing_card(ORIG_DECK_LEFT, MOVEMENT)]
      #ODDS_OF_DRAWING_CASE_MOVEMENTS_BY_TEAM = capped mixed strategy weight
      join_track_left[, MIX_STRATEGY_CAP := prod(ODDS_OF_DRAWING_MOVEMENT), by = .(case_id, TEAM_ID)]
      join_track_left[, ':=' (CYCLERS = paste0(CYCLER_ID, collapse = "_"),
                              MOVES = paste0(MOVEMENT, collapse = "_")), by = .(case_id, TEAM_ID)]
      #calculate new odds based on expect moves of others

      join_track_left[case_id == 1]
      res <- calc_OTHER_ACTION_ODDS(join_track_left)
      res <- calc_OTHER_ACTION_ODDS(res)


      aggr_to_team <- join_track_left[, .(CYCLERS , TEAM_SCORE = sum(TOT_SCORE), MOVES), by = .(TEAM_ID, case_id, opponent_moves, case_odds)]

    #to lp solver


    res <- calc_OTHER_ACTION_ODDS(res)

    check_res <- res[, .(CYCLERS, TEAM_SCORE = sum(TOT_SCORE), MOVES), by = .(TEAM_ID, case_id, opponent_moves, PROB_PRODUCT, OTHER_ACTION_ODDS, MIX_STRATEGY_CAP)]
    # check_res[MOVES == "7_9" & TEAM_ID == 1, sum(OTHER_ACTION_ODDS)]
    # check_res[MOVES == "7_9" & TEAM_ID == 1]#, sum(OTHER_ACTION_ODDS)]
    # strategies <- check_res[, .N, by = .(TEAM_ID, MOVES)]
    # strategies[, moves_sort_rank := .GRP, by = MOVES]
    # strategies[, sort_score := ( (moves_sort_rank) * (TEAM_ID * 2)^10)]
    # strategies[order(sort_score)]
    # join_sort_rank <- strategies[check_res, on = .(TEAM_ID, MOVES)]



    check_res2 <- check_res[, .(EV = sum(TEAM_SCORE * OTHER_ACTION_ODDS) / sum(OTHER_ACTION_ODDS)), by = .(CYCLERS, MOVES, TEAM_ID, MIX_STRATEGY_CAP)][order(-EV)]
    check_res2[, rank_ev := frank(-EV), by = TEAM_ID]

    check_res2[TEAM_ID == 3]

    #calc new mixed strategy
    gamma <- 0.9
    check_res2[, top := exp(1/gamma*EV) ]
    check_res2[, bottom := sum(top), by = TEAM_ID]
    check_res2[, new_prob_uncapped := top / bottom]
    check_res2[, new_prob_capped := pmin(new_prob_uncapped, MIX_STRATEGY_CAP)]

    #now need to scale back to 1
    check_res2[, missing_prob :=  1 - sum(new_prob_capped), by = TEAM_ID]
    check_res2[, non_missing_prob_mass :=  sum(ifelse(new_prob_capped == MIX_STRATEGY_CAP, 0, new_prob_capped)), by = TEAM_ID]
    #scaling the uncapped
    check_res2[, new_prob := ifelse(new_prob_capped == MIX_STRATEGY_CAP, new_prob_capped, new_prob_uncapped  + new_prob_uncapped /  non_missing_prob_mass * missing_prob)]
    #WE MIGHT GET OVER MIX STRAT AGAIN AFTER SCALING. NEED TO BUILD LOOPING SOLVER
    check_res2[TEAM_ID == 3]
    ######################
    check_res2[, sum(new_prob), by = TEAM_ID]
    mixed_strategy <- check_res2[, .(CYCLERS, MOVES, TEAM_ID, new_prob)]

    #now calculate EV again
    # mixed_strategy[, c("CYC1", "CYC2") := tstrsplit(CYCLERS, "_", fixed=TRUE), by = .(CYCLERS, MOVES, TEAM_ID)]
    # mixed_strategy[, c("MOV1", "MOV2") := tstrsplit(MOVES, "_", fixed=TRUE), by = .(CYCLERS, MOVES, TEAM_ID)]
    # #meltta
    # long_format <- rbind(mixed_strategy[, .(CYCLER_ID = as.numeric(CYC1), MOVEMENT = as.numeric(MOV1), TEAM_ID, new_prob)],
    #                      mixed_strategy[, .(CYCLER_ID = as.numeric(CYC2), MOVEMENT = as.numeric(MOV2), TEAM_ID, new_prob)])

    join_new_strat <- mixed_strategy[res, on = .(CYCLERS, MOVES, TEAM_ID)]
    join_new_strat[, NEW_CASE_PROB := prod(new_prob)]
    check_res <- join_new_strat[, .(TEAM_SCORE = sum(TOT_SCORE)), by = .(MOVES, CYCLERS, TEAM_ID, case_id, opponent_moves, new_prob, MIX_STRATEGY_CAP)]


    check_res[, CASE_PROB := prod(new_prob), by = case_id]
    check_res[, OPPONENT_CASE_PROB := prod(new_prob) / new_prob, by = case_id]
    check_res[case_id == 4212]
   #JÄIT MIETTIIN, ETTÄ KUULUKO EV:SEN OMA TN VAI VAAN MUITTEN TARJOOMA?! LUULISIN; ETTÄ OMA EI KUULU
    check_res[, WEIGHTED_SCORE := OPPONENT_CASE_PROB * TEAM_SCORE]
    check_res[order(WEIGHTED_SCORE)]
    check_res2 <- check_res[, .(EV = sum(WEIGHTED_SCORE)), by = .(CYCLERS, MOVES, TEAM_ID, MIX_STRATEGY_CAP)][order(-EV)]
    check_res2[TEAM_ID == 3][ORDER(-EV)]


    #DEL ME LATER
    gamma <- 0.3
    check_res2[, top := exp(1/gamma*EV) ]
    check_res2[, bottom := sum(top), by = TEAM_ID]
    check_res2[, new_prob_uncapped := top / bottom]
    check_res2[, new_prob_capped := pmin(new_prob_uncapped, MIX_STRATEGY_CAP)]

    #now need to scale back to 1
    check_res2[, missing_prob :=  1 - sum(new_prob_capped), by = TEAM_ID]
    check_res2[, non_missing_prob_mass :=  sum(ifelse(new_prob_capped == MIX_STRATEGY_CAP, 0, new_prob_capped)), by = TEAM_ID]
    #scaling the uncapped
    check_res2[, new_prob := ifelse(new_prob_capped == MIX_STRATEGY_CAP, new_prob_capped, new_prob_uncapped  + new_prob_uncapped /  non_missing_prob_mass * missing_prob)]
    #WE MIGHT GET OVER MIX STRAT AGAIN AFTER SCALING. NEED TO BUILD LOOPING SOLVER
    check_res2[TEAM_ID == 3]
    check_res2[, sum(new_prob), by = TEAM_ID ]
    #END HERE







    zoom(game_status)


    orig <- join_track_left[, .(CYCLERS = paste0(CYCLER_ID, collapse = "_"), TEAM_SCORE = sum(TOT_SCORE), MOVES = paste0(MOVEMENT, collapse = "_")), by = .(TEAM_ID, case_id, opponent_moves, PROB_PRODUCT, OTHER_ACTION_ODDS)]
    orig2 <- orig[, .(EV = sum(TEAM_SCORE * OTHER_ACTION_ODDS) / sum(OTHER_ACTION_ODDS), ORIG_EV = mean(TEAM_SCORE)), by = .(CYCLERS, MOVES, TEAM_ID)][order(-EV)]
    orig2[, rank_ev := frank(-EV), by = TEAM_ID]
    orig2[, rank_ev_orig := frank(-ORIG_EV), by = TEAM_ID]
    orig2[TEAM_ID == 3]

    res2 <- calc_OTHER_ACTION_ODDS(res)


    orig4 <- res2[, .(CYCLERS = paste0(CYCLER_ID, collapse = "_"), TEAM_SCORE = sum(TOT_SCORE), MOVES = paste0(MOVEMENT, collapse = "_")), by = .(TEAM_ID, case_id, opponent_moves, PROB_PRODUCT, OTHER_ACTION_ODDS)]
    orig5 <- orig4[, .(EV = sum(TEAM_SCORE * OTHER_ACTION_ODDS) / sum(OTHER_ACTION_ODDS), ORIG_EV = mean(TEAM_SCORE)), by = .(CYCLERS, MOVES, TEAM_ID)][order(-EV)]
    orig5[, rank_ev := frank(-EV), by = TEAM_ID]
    orig5[, rank_ev_orig := frank(-ORIG_EV), by = TEAM_ID]
    orig5[TEAM_ID == 3]

   #
   #
   #  worse_move <- aggr_to_team[, .(
   #                                 EV = mean(TEAM_SCORE)), by = .(CYCLERS, MOVES, TEAM_ID)][order(-EV)]
   #
   #  worse_move[, M1 := str_sub(MOVES, 1, 1)]
   #  worse_move[, M2 := str_sub(MOVES, 3, 3)]
   #  worse_move[, C1 := str_sub(CYCLERS, 1, 1)]
   #  worse_move[, C2 := str_sub(CYCLERS, 3, 3)]
   #  #aggr move1
   #  move1_aggr <- worse_move[, .(EV = mean(EV),
   #                                VAR = var(EV)), by = .(TEAM_ID, MOVEMENT = as.numeric(M1), CYCLER_ID = as.numeric(C1))]
   #  aggr_card_count <- deck_status[Zone != "Removed", .(count = .N), by = .(CYCLER_ID, MOVEMENT)]
   #  join_count1 <- aggr_card_count[move1_aggr, on = .(MOVEMENT, CYCLER_ID)][order(TEAM_ID, -EV)]
   #  join_count1[, PRIO_GROUP := seq_len(.N), by = .(TEAM_ID)]
   #  join_count1[, draw_odds := exact_draw_odds_outer_vectorized(draw_odds_data = .SD), by = TEAM_ID, .SDcols = c("PRIO_GROUP", "count")]
   #  weighted_variance <- join_count1[, .(CYCLER_ID = mean(CYCLER_ID), WVAR = sum(VAR * draw_odds) / sum(draw_odds)), by = .(TEAM_ID)]
   #
   #  move2_aggr <- worse_move[, .(EV = mean(EV),
   #                               VAR = var(EV)), by = .(TEAM_ID, MOVEMENT = as.numeric(M2), CYCLER_ID = as.numeric(C2))]
   #  join_count2 <- aggr_card_count[move2_aggr, on = .(MOVEMENT, CYCLER_ID)][order(TEAM_ID, -EV)]
   #  join_count2[, PRIO_GROUP := seq_len(.N), by = .(TEAM_ID)]
   #  join_count2[, draw_odds := exact_draw_odds_outer_vectorized(draw_odds_data = .SD), by = TEAM_ID, .SDcols = c("PRIO_GROUP", "count")]
   #  weighted_variance2 <- join_count2[, .(CYCLER_ID = mean(CYCLER_ID), WVAR = sum(VAR * draw_odds) / sum(draw_odds)), by = .(TEAM_ID)]
   #  append_vars <- rbind(weighted_variance2, weighted_variance)
   #  first_cycler_per_team <- append_vars[ , .SD[which.min(WVAR)], by = TEAM_ID]
   #
   #
   #  append_long <- rbind(join_count1, join_count2)
   #  #join first cyc
   #  join_first_cyc <- first_cycler_per_team[append_long, on = .(TEAM_ID, CYCLER_ID),. (WVAR, MOVEMENT, CYCLER_ID, TEAM_ID, draw_odds_1 = draw_odds, PRIO_GROUP_1 = PRIO_GROUP)]
   #
   #  #make worse move to long format
   #  worse_move1 <- worse_move[, .(MOVEMENT = as.numeric(M1), CYCLER_ID = as.numeric(C1), OTHER_MOVE = as.numeric(M2), OTHER_CYCLER = as.numeric(C2), TEAM_ID, EV)]
   #  worse_move2 <- worse_move[, .(MOVEMENT = as.numeric(M2), CYCLER_ID = as.numeric(C2), OTHER_MOVE = as.numeric(M1),  OTHER_CYCLER = as.numeric(C1),TEAM_ID, EV)]
   #  append_worse <- rbind(worse_move1, worse_move2)
   #  join_first_cyc_long <- join_first_cyc[append_worse, on = .(TEAM_ID, CYCLER_ID, MOVEMENT)][!is.na(WVAR)]
   #  #calc second prio grou
   #  setorder(join_first_cyc_long, TEAM_ID, PRIO_GROUP_1, -EV)
   #  join_first_cyc_long[, PRIO_GROUP_2 := seq_len(.N), by = .(TEAM_ID, PRIO_GROUP_1)]
   #  #join card count again
   #  rename_card_data <- aggr_card_count[, .(OTHER_CYCLER = CYCLER_ID, OTHER_MOVE = MOVEMENT, count_2nd = count)]
   #  join_card_count <- rename_card_data[join_first_cyc_long, on = .(OTHER_CYCLER, OTHER_MOVE)]
   #
   #  join_card_count[, draw_odds_2 := exact_draw_odds_outer_vectorized(.SD), by = .(TEAM_ID, PRIO_GROUP_1), .SDcols = c("PRIO_GROUP_2", "count_2nd")]
   #  join_card_count[, PROPABILITY := draw_odds_1 * draw_odds_2]
   #  join_card_count[, prob_case_id := seq_len(.N)]
   #  back_to_long_1 <- join_card_count[, .(CYCLER_ID, MOVEMENT, prob_case_id, PROPABILITY, TEAM_ID)]
   #  back_to_long_2 <- join_card_count[, .(CYCLER_ID = OTHER_CYCLER, MOVEMENT = OTHER_MOVE, prob_case_id, PROPABILITY, TEAM_ID)]
   #  result_long <- rbind(back_to_long_1, back_to_long_2)
   #  setorder(result_long, TEAM_ID, prob_case_id, CYCLER_ID)
   #
   # # join_back_to_track_left <- result_long[join_track_left, on = .(CYCLER_ID, TEAM_ID, MOVEMENT)]
   #
   #  aggregate_result <- result_long[, .(CYCLERS = paste0(CYCLER_ID, collapse = "_"), MOVES = paste0(MOVEMENT, collapse = "_")),
   #                                  by = .(prob_case_id, TEAM_ID, PROPABILITY)][, prob_case_id := NULL]
   #  #ready to join!
   #  join_prob_to_cases <- aggregate_result[aggr_to_team, on = .(TEAM_ID, CYCLERS, MOVES)]
   #  join_prob_to_cases[, helper_odds_for_other_odds_calc := ifelse(PROPABILITY == 0, 0.000000001, PROPABILITY)]
   #  join_prob_to_cases[, ':=' (OTHER_ACTION_ODDS = (prod(helper_odds_for_other_odds_calc) / helper_odds_for_other_odds_calc),
   #                     PROB_PRODUCT = prod(PROPABILITY)), by = case_id]
   #
   #  sscols_case <- join_prob_to_cases[, .(PROB_PRODUCT, OTHER_ACTION_ODDS, case_id, TEAM_ID)]
   #
   #  join_prob_to_cases[, sum(PROPABILITY), by = .(opponent_moves, TEAM_ID)]
   #  join_prob_to_cases[TEAM_ID == 1 & MOVES == "3_4" & PROPABILITY > 0]
   #  #finally join case prob
   #  case_prob <- sscols_case[join_track_left, on = .(case_id, TEAM_ID)]
   #  case_prob[case_id == 14875]
   #






    #recalculate
    filtered_aggr_to_team <- aggr_to_team[accpeted_moves, on = .(MOVES, TEAM_ID)]
    filtered_aggr_to_team[, SCORE_RANK := frank(-TEAM_SCORE), by = .(TEAM_ID, opponent_moves)]
    worse_move2 <- filtered_aggr_to_team[, mean(SCORE_RANK), by = .(MOVES, TEAM_ID)][order(V1)]
    worse_move2[, ratio := V1 / .N, by = TEAM_ID]
    accpeted_moves2 <- worse_move2[ratio < 0.75, .(MOVES, TEAM_ID)]

    #recalc loop
    for (loop in 1:10) {
      filtered_aggr_to_team <- filtered_aggr_to_team[  accpeted_moves2 , on = .(MOVES, TEAM_ID)]
      filtered_aggr_to_team[, SCORE_RANK := frank(-TEAM_SCORE), by = .(TEAM_ID, opponent_moves)]
      worse_move2 <- filtered_aggr_to_team[, mean(SCORE_RANK), by = .(MOVES, TEAM_ID)][order(V1)]
      worse_move2[, ratio := V1 / .N, by = TEAM_ID]
      accpeted_moves2 <- worse_move2[ratio < 0.75, .(MOVES, TEAM_ID)]
     print(accpeted_moves2[, .N])
    }



  #join track
  track_ss <- pre_track$aggr_to_slots[, .(new_cyc_pos = GAME_SLOT_ID, TRACK_LEFT)]
  join_track_left <- track_ss[join_info, on = .(new_cyc_pos)]
  join_track_left[!is.na(TRACK_LEFT), TURNS_TO_FINISH := as.double(finish_turns_db(con, TRACK_LEFT, DECK_LEFT, pre_track, new_cyc_pos,
                                                                                   draw_odds_raw_data = DRAW_ODDS, save_to_DB = TRUE)),
                  by = .(new_cyc_pos, TRACK_LEFT, DECK_LEFT, DRAW_ODDS)]




  #now we score, then guess opponents moves, then we add draw odds, then we choose our best move

  #add draw_odds

  onlycalced <- join_track_left[!is.na(TURNS_TO_FINISH)]

  rm(ADM_OPTIMAL_MOVES)
  required_data("ADM_OPTIMAL_MOVES")


  #start creating KPIs

  #TTS
  onlycalced[, leader_turns :=  min(TURNS_TO_FINISH), by = case_id]
  onlycalced[, second_turns := min(ifelse(TURNS_TO_FINISH > leader_turns, TURNS_TO_FINISH, 1000))]
  onlycalced[, is_leader := TURNS_TO_FINISH == leader_turns]
  onlycalced[, my_min_ttf := min(TURNS_TO_FINISH), by = .(CYCLER_ID, case_id)]
  onlycalced[, ttf_score :=  my_min_ttf - TURNS_TO_FINISH  + as.numeric(is_leader) * 0.5, by = .(CYCLER_ID, case_id)]

  #EXHAUST
  onlycalced[, exhaust_free_slots := list(list(new_cyc_pos - 1)), by = case_id]
  onlycalced[, exhaust_added := !new_cyc_pos %in% exhaust_free_slots[[1]], by = .(CYCLER_ID, case_id)]
  onlycalced[, exhaust_score := -exhaust_added * TURNS_TO_FINISH / 15 ]

  #CYCLER_DISTANCE
  onlycalced[, team_penalty_score := (pmax(mean(new_cyc_pos) - new_cyc_pos, 1) ^ (1 / 2) - 1) / 2, by = .(TEAM_ID, case_id)]

  #FINISH SCORE
  finish_slot <- pre_track$aggr_to_slots[FINISH == 1, GAME_SLOT_ID]
  onlycalced[, over_finish_square := max(new_cyc_pos - finish_slot, 0) * MOVEMENT * 100]

  onlycalced[, actual_movement := (new_cyc_pos - curr_pos)]
  onlycalced[, move_diff := actual_movement - MOVEMENT]
  #calculate score

  cyc_dist_eff <- ADM_AI_CONF[CYCLER_ID == 6  & Setting == "Cycler_distance", Value]
  exh_eff <- ADM_AI_CONF[CYCLER_ID == 6 & Setting == "Get_exhaust", Value]
  speed_eff <- ADM_AI_CONF[CYCLER_ID == 6 & Setting == "Speed_change", Value]
  move_gai_eff <- ADM_AI_CONF[CYCLER_ID == 6 & Setting == "Movement_gained", Value]
  card_spend_eff <-  ADM_AI_CONF[CYCLER_ID == 6 & Setting == "Card_spent", Value]



  onlycalced[, row_id := seq_len(.N)]
  onlycalced[, Result :=
               over_finish_square +
               exhaust_score * exh_eff +
               ttf_score * speed_eff +
               actual_movement * move_gai_eff * 1.1+
               -MOVEMENT * card_spend_eff
             ,by = .(case_id, CYCLER_ID)]

  sscols_res <- onlycalced[, .(case_id, CYCLER_ID, TEAM_ID, MOVEMENT, Result, case_odds, ttf_score, exhaust_score, team_penalty_score, move_diff)]

  aggre <- sscols_res[CYCLER_ID %in% c(moving_cycler, second_cycler), .(team_Result = mean(Result)), by = .( case_id)]#[order(CYCLER_ID, MOVEMENT)]

  join_team_res <- aggre[sscols_res, on = "case_id"]
  choose_move <- join_team_res[CYCLER_ID == moving_cycler, mean(team_Result), by = MOVEMENT]
  max_score <- choose_move[, max(V1)]


  res_move <- max(choose_move[V1 == max_score, MOVEMENT])
}
return(res_move)
}
# sscols[, CJ(edge1 = individual, edge2 = individual), by = group][edge1 < edge2]
#
#
# sscols
# setkey(sscols, CYCLER_ID, MOVEMENT)
# res <- sscols[CJ(CYCLER_ID, MOVEMENT, unique = TRUE)
#     ][, lapply(.SD, sum), by = .(CYCLER_ID, MOVEMENT)
#       ][is.na(odds), odds := 0]
