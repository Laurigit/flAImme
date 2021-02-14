calulate_mixed_strategy_inner_capped <- function(EV_TABLE, combinations_data, deck_status, ADM_CYCLER_INFO, input_gamma = 0.9 ) {
  #test_calculate_mix_strategy_cap.R

  #combinations_data <- join_track_left
  worse_move <- EV_TABLE#[TEAM_ID == 1]

  worse_move[, M1 := str_sub(MOVES, 1, 1)]
  worse_move[, M2 := str_sub(MOVES, 3, 3)]
  worse_move[, C1 := str_sub(CYCLERS, 1, 1)]
  worse_move[, C2 := str_sub(CYCLERS, 3, 3)]
  move1_aggr <- worse_move[, .(WEIGH_EV = sum((EV * PROB_PRODUCT)) / sum(PROB_PRODUCT)), by = .(TEAM_ID, MOVEMENT = as.numeric(M1), CYCLER_ID = as.numeric(C1))]
  weighted_variance <- move1_aggr[, .(CYCLER_ID = mean(CYCLER_ID), WVAR = var(WEIGH_EV)), by = .(TEAM_ID)]

  move2_aggr <- worse_move[, .(WEIGH_EV = sum((EV * PROB_PRODUCT)) / sum(PROB_PRODUCT)), by = .(TEAM_ID, MOVEMENT = as.numeric(M2), CYCLER_ID = as.numeric(C2))]

  weighted_variance2 <- move2_aggr[, .(CYCLER_ID = mean(CYCLER_ID), WVAR = var(WEIGH_EV)), by = .(TEAM_ID)]
  total_ev_data <- rbind(move1_aggr, move2_aggr)
  append_vars <- rbind(weighted_variance2, weighted_variance)
  first_cycler_per_team <- append_vars[ , .SD[which.min(WVAR)], by = TEAM_ID]

  first_cycler_ev <- total_ev_data[CYCLER_ID %in% first_cycler_per_team[, CYCLER_ID]]
  gamma <- input_gamma
  first_cycler_ev[, top := exp(1 / gamma * WEIGH_EV) ]
  first_cycler_ev[, bottom := sum(top), by = TEAM_ID]
  first_cycler_ev[, target_strat := top / bottom]
  aggr_card_count <- deck_status[Zone != "Removed", .(count = .N), by = .(CYCLER_ID, MOVEMENT)]
  join_count1 <- aggr_card_count[first_cycler_ev, on = .(MOVEMENT, CYCLER_ID)]#[order(TEAM_ID, - new_prob_uncapped)]
  #join_count1[, draw_odds := exact_draw_odds_outer_vectorized(draw_odds_data = .SD), by = TEAM_ID, .SDcols = c("PRIO_GROUP", "count")]
  join_count1[, mixed_cap := 1 - suppressWarnings(phyper(0, count, sum(count) - count, 4)), by = CYCLER_ID]
  join_count1[, OTHER_MOVE := 8] #this is here to fit into the general function. 8 is funny number as we don't have a card for that
  join_count1[, strat_over_cap := mixed_cap < target_strat]
  join_count1[, recalculate := max(strat_over_cap), by = .(TEAM_ID, OTHER_MOVE)]
  #THIS SORT IS ENEEDED!!
  setorder(join_count1, TEAM_ID, MOVEMENT, OTHER_MOVE)
  #############

  join_count1[recalculate == TRUE, capped_strat := constrained_mixed_strategy_long(.SD),
              by = .(TEAM_ID, OTHER_MOVE), .SDcols = c("TEAM_ID", "MOVEMENT", "OTHER_MOVE", "mixed_cap", "target_strat")]

  join_count1[, strategy := ifelse(is.na(capped_strat), target_strat, capped_strat)]

  join_count1[, OTHER_MOVE := NULL]
  #aggr_rs <- total_res[, .N, by = .(TEAM_ID, MOVEMENT, strategy)]
  join_to_input <- join_count1[join_count1, on = .(TEAM_ID, MOVEMENT)]
  ss_first_result <- join_to_input[, .(TEAM_ID, FIRST_MOVEMENT = MOVEMENT,
                                       FIRST_P = strategy,
                                       FIRST_CYCLER = CYCLER_ID)]


  #make worse move to long format
  worse_move1 <- worse_move[, .(MOVEMENT = as.numeric(M1), CYCLER_ID = as.numeric(C1), OTHER_MOVE = as.numeric(M2), OTHER_CYCLER = as.numeric(C2), TEAM_ID, EV)]
  worse_move2 <- worse_move[, .(MOVEMENT = as.numeric(M2), CYCLER_ID = as.numeric(C2), OTHER_MOVE = as.numeric(M1),  OTHER_CYCLER = as.numeric(C1),TEAM_ID, EV)]
  append_worse <- rbind(worse_move1, worse_move2)
  second_cycler_data <- append_worse[!CYCLER_ID %in% first_cycler_per_team[, CYCLER_ID]]
  join_card_count_to_second <- aggr_card_count[second_cycler_data, on = .(MOVEMENT, CYCLER_ID)]#[order(TEAM_ID, - new_prob_uncapped)]
  join_card_count_to_second[, mixed_cap := 1 - suppressWarnings(phyper(0, count, sum(count) - count, 4)), by = .(CYCLER_ID, OTHER_MOVE)]
  join_card_count_to_second[, top := exp(1 / gamma * EV) ]
  join_card_count_to_second[, bottom := sum(top), by = .(CYCLER_ID, OTHER_MOVE)]
  join_card_count_to_second[, target_strat := top / bottom]
  #join_card_count_to_second[, capped_strategy := constrained_mixed_strategy_long(.SD), by = .(TEAM_ID, OTHER_MOVE), .SDcols = c("MOVEMENT", "target_strat", "mixed_cap")]
  join_card_count_to_second[, strat_over_cap := mixed_cap > target_strat]
  join_card_count_to_second[, recalculate := max(strat_over_cap), by = .(TEAM_ID, OTHER_MOVE)]
  #THIS SORT IS ENEEDED!!
  setorder(join_card_count_to_second, TEAM_ID, MOVEMENT, OTHER_MOVE)
  #############

  join_card_count_to_second[recalculate == TRUE, capped_strat := constrained_mixed_strategy_long(.SD),
              by = .(TEAM_ID, OTHER_MOVE), .SDcols = c("TEAM_ID", "MOVEMENT", "OTHER_MOVE", "mixed_cap", "target_strat")]
  join_card_count_to_second[, strategy := ifelse(is.na(capped_strat), target_strat, capped_strat)]


  ss_res_second <- join_card_count_to_second[, .(SECOND_CYCLER = CYCLER_ID, TEAM_ID, SECOND_MOVEMENT = MOVEMENT, SECOND_P = strategy, FIRST_MOVEMENT = OTHER_MOVE)]

  join_first_cyc <- ss_first_result[ss_res_second, on = .(TEAM_ID, FIRST_MOVEMENT)]
  join_first_cyc[, TOTAL_P := FIRST_P * SECOND_P]
  join_first_cyc[, comb_id := seq_len(.N)]
  rouler_ids <- ADM_CYCLER_INFO[CYCLER_TYPE_NAME == "Rouler", CYCLER_ID]
  sprint_ids <- ADM_CYCLER_INFO[CYCLER_TYPE_NAME == "Sprinteur", CYCLER_ID]
  roulers_player_first <- join_first_cyc[FIRST_CYCLER %in% rouler_ids][, .(MOVEMENT = FIRST_MOVEMENT,
                                                                           CYCLER_ID = FIRST_CYCLER,
                                                                           PROB_ROULER = FIRST_P,
                                                                           TOTAL_P,
                                                                           TEAM_ID,
                                                                           comb_id)]
  rouler_played_second <- join_first_cyc[SECOND_CYCLER %in% rouler_ids][, .(MOVEMENT = SECOND_MOVEMENT,
                                                                            CYCLER_ID = SECOND_CYCLER,
                                                                            PROB_ROULER = SECOND_P,
                                                                            TOTAL_P,
                                                                            TEAM_ID,
                                                                            comb_id)]
  sprinter_player_first <- join_first_cyc[FIRST_CYCLER %in% sprint_ids][, .(SPRINT_MOVEMENT = FIRST_MOVEMENT,
                                                                            SPRINT_CYCLER_ID = FIRST_CYCLER,
                                                                            PROB_SPRINTER = FIRST_P,
                                                                            comb_id)]
  sprinter_played_second <- join_first_cyc[SECOND_CYCLER %in% sprint_ids][, .(SPRINT_MOVEMENT = SECOND_MOVEMENT,
                                                                              SPRINT_CYCLER_ID = SECOND_CYCLER,
                                                                              PROB_SPRINTER = SECOND_P,
                                                                              comb_id)]
  append_roulers <- rbind(roulers_player_first, rouler_played_second)
  append_sprint <- rbind(sprinter_player_first, sprinter_played_second)
  join_data <- append_roulers[append_sprint, on = "comb_id"]
  combined_data <- join_data[, .(CYCLERS = paste0(CYCLER_ID, "_", SPRINT_CYCLER_ID),
                             MOVES = paste0(MOVEMENT, "_", SPRINT_MOVEMENT),
                             TEAM_ID,
                             new_prob = TOTAL_P)]

  join_new_strat <- combined_data[combinations_data, on = .(CYCLERS, MOVES, TEAM_ID)]
 # join_new_strat[case_id == 1]
  # join_new_strat[, NEW_CASE_PROB := prod(new_prob)]
  current_ev_data <- join_new_strat[, .(TEAM_SCORE = sum(TOT_SCORE)), by = .(MOVES, CYCLERS, TEAM_ID, case_id, new_prob)]

  current_ev_data[, WEIGHTED_SCORE := prod(new_prob) / new_prob * TEAM_SCORE, by = case_id]
 # current_ev_data[case_id == 1]
  # current_ev_data[, OPPONENT_CASE_PROB := prod(new_prob) / new_prob, by = case_id]

  #current_ev_data[, WEIGHTED_SCORE := OPPONENT_CASE_PROB * TEAM_SCORE]

  result <- current_ev_data[, .(EV = sum(WEIGHTED_SCORE)), by = .(CYCLERS, MOVES, TEAM_ID, PROB_PRODUCT = new_prob)]
  return(result)


}
