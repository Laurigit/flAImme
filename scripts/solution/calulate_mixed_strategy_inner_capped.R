calulate_mixed_strategy_inner_capped <- function(EV_TABLE, combinations_data, static_model = NULL, ADM_CYCLER_INFO, input_gamma = 0.9, cap_data = NULL ) {
  #test_calculate_mix_strategy_cap.R

 #  EV_TABLE <- check_res2
 #  combinations_data <- join_track_left
 # input_gamma = first_round_gamma
 # cap_data = cap_data
 #
  #combinations_data <- join_track_left
  worse_move <- EV_TABLE#[TEAM_ID == 1]

  one_cycler_teams <- worse_move[is.na(C2), .N, by = TEAM_ID]

  move1_aggr <- worse_move[, .(WEIGH_EV = sum(EV * PROB_PRODUCT, na.rm = TRUE) / sum(PROB_PRODUCT, na.rm = TRUE)), by = .(TEAM_ID, MOVEMENT = (M1), CYCLER_ID = (C1))]
  weighted_variance <- move1_aggr[, .(CYCLER_ID = mean(CYCLER_ID), WVAR = var(WEIGH_EV)), by = .(TEAM_ID)]
  weighted_variance[, WVAR := ifelse(is.na(WVAR), 1, WVAR)]
  move2_aggr <- worse_move[!is.na(M2), .(WEIGH_EV = sum((EV * PROB_PRODUCT)) / sum(PROB_PRODUCT)), by = .(TEAM_ID, MOVEMENT = (M2), CYCLER_ID = (C2))]

  weighted_variance2 <- move2_aggr[, .(CYCLER_ID = mean(CYCLER_ID), WVAR = var(WEIGH_EV)), by = .(TEAM_ID)]
  weighted_variance2[, WVAR := ifelse(is.na(WVAR), 1, WVAR)]
  total_ev_data <- rbind(move1_aggr, move2_aggr)
  append_vars <- rbind(weighted_variance2, weighted_variance)
  first_cycler_per_team <- append_vars[ , .SD[which.min(WVAR)], by = TEAM_ID]
  second_cycler_per_team <- append_vars[!CYCLER_ID %in% first_cycler_per_team[, CYCLER_ID]]
  first_cycler_ev <- total_ev_data[CYCLER_ID %in% first_cycler_per_team[, CYCLER_ID]]
  gamma <- input_gamma
  first_cycler_ev[, top := exp(1 / gamma * WEIGH_EV) ]
  first_cycler_ev[, bottom := sum(top), by = TEAM_ID]
  first_cycler_ev[, target_strat := round(top / bottom, 5)]

  if (!is.finite(first_cycler_ev[, max(top)])) {warning("fix mee, infinite top")}

  first_cycler_ev[, OTHER_MOVE := 8] #this is here to fit into the general function. 8 is funny number as we don't have a card for that

  #this sets to zero unused variables
  other_move_vec <-   setdiff(2:9, first_cycler_ev[, .N, by = OTHER_MOVE][, OTHER_MOVE])

  if (nrow(first_cycler_ev) == 0) {browser()}


join_cap <- cap_data[first_cycler_ev, on = .(CYCLER_ID, MOVEMENT)]
sorted <- join_cap[order(CYCLER_ID, -target_strat)]

sorted[, prio := seq_len(.N), by = .(CYCLER_ID)]
max_prio <- sorted[, max(prio)]
sorted[, PORB_MASS_LEFT := ifelse(prio == 1, 1, 0)]
for (prio_loop in 1:max_prio) {

  sorted[, P_TO_FILL := ifelse(prio == prio_loop, PORB_MASS_LEFT, 0)]
  sorted[prio == prio_loop, P_MASS_FILLED := pmin(P_TO_FILL, mixed_cap, target_strat)]
  sorted[, PORB_MASS_LEFT := ifelse(prio == prio_loop + 1, 1 - sum(P_MASS_FILLED, na.rm = TRUE), 0), by = CYCLER_ID]

}

sorted[, ':=' (top = NULL, bottom = NULL, PORB_MASS_LEFT = NULL, P_TO_FILL = NULL)]
sorted[, LEFTOVER := 1 - sum(P_MASS_FILLED), by = .(CYCLER_ID, OTHER_MOVE)]
sorted[, ROOM_TO_FILL := mixed_cap - P_MASS_FILLED]

for (prio_loop in 2:max_prio) {
  sorted[, MAX_FILL_FOR_THIS_LOOP := min(ifelse(ROOM_TO_FILL == 0, 10000, ROOM_TO_FILL)), by = .(CYCLER_ID, OTHER_MOVE)]
  sorted[, FILLED_MOVEMENTS_LOOP_COUNT := sum(ifelse(ROOM_TO_FILL > 0, 1, 0)), by = .(CYCLER_ID, OTHER_MOVE)]
  sorted[ROOM_TO_FILL > 0, P_MASS_FILLED := P_MASS_FILLED + pmin(MAX_FILL_FOR_THIS_LOOP, LEFTOVER / FILLED_MOVEMENTS_LOOP_COUNT)]
  sorted[, LEFTOVER := 1 - sum(P_MASS_FILLED), by = .(CYCLER_ID, OTHER_MOVE)]
  # sorted[, FILL_AMOUNT_LOOP := ifelse(prio >= prio_loop, )
}
total_res <- sorted[, .(CYCLER_ID, MOVEMENT, OTHER_MOVE, strategy = P_MASS_FILLED)]

#  total_res <- constrained_mixed_strategy_long_target(first_cycler_ev, static_model, second_cycler_per_team[, CYCLER_ID], other_move_vec)

  #let's fix NA with 0
  total_res[, strategy := ifelse(is.na(strategy), 0.00001, strategy)]
  total_res[, OTHER_MOVE := NULL]
  #aggr_rs <- total_res[, .N, by = .(TEAM_ID, MOVEMENT, strategy)]

  join_to_input <- total_res[first_cycler_ev, on = .(CYCLER_ID, MOVEMENT)]
  join_to_input[, strategy := ifelse(is.na(strategy), 0, strategy)]
  ss_first_result <- join_to_input[, .(TEAM_ID, FIRST_MOVEMENT = MOVEMENT,
                                       FIRST_P = strategy,
                                       FIRST_CYCLER = CYCLER_ID)]


  #make worse move to long format
  worse_move1 <- worse_move[, .(MOVEMENT = as.numeric(M1), CYCLER_ID = as.numeric(C1), OTHER_MOVE = as.numeric(M2), OTHER_CYCLER = as.numeric(C2), TEAM_ID, EV)]
  worse_move2 <- worse_move[!is.na(C2), .(MOVEMENT = as.numeric(M2), CYCLER_ID = as.numeric(C2), OTHER_MOVE = as.numeric(M1),  OTHER_CYCLER = as.numeric(C1),TEAM_ID, EV)]
  append_worse <- rbind(worse_move1, worse_move2)
  join_card_count_to_second <- append_worse[CYCLER_ID %in% second_cycler_per_team[, CYCLER_ID]]#[!is.na()]
  #join_card_count_to_second <- aggr_card_count[second_cycler_data, on = .(MOVEMENT, CYCLER_ID)]#[order(TEAM_ID, - new_prob_uncapped)]
  join_card_count_to_second[, top := exp(1 / gamma * EV) ]
  join_card_count_to_second[, bottom := sum(top), by = .(CYCLER_ID, OTHER_MOVE)]
  join_card_count_to_second[, target_strat := round(top / bottom, 5)]


  #this is for rbind later
  combined_data <- NULL
  one_cycler_team_result <- NULL
  #join_card_count_to_second[, capped_strategy := constrained_mixed_strategy_long(.SD), by = .(TEAM_ID, OTHER_MOVE), .SDcols = c("MOVEMENT", "target_strat", "mixed_cap")]
  if (nrow(join_card_count_to_second) == 0 | nrow(one_cycler_teams) > 0) {
    one_cycler_team_result <- ss_first_result[TEAM_ID %in% one_cycler_teams[, TEAM_ID], .(CYCLERS = as.character(FIRST_CYCLER),
                                   MOVES = as.character(FIRST_MOVEMENT) ,
                                   TEAM_ID,
                                   new_prob = FIRST_P ,
                                   C1 = FIRST_CYCLER, C2 = NA, M1 = FIRST_MOVEMENT, M2 = NA)]
  }
  if (nrow(join_card_count_to_second) > 0) {
  other_move_vec2 <- setdiff(2:9, join_card_count_to_second[, .N, by = OTHER_MOVE][, OTHER_MOVE])

  join_cap <- cap_data[join_card_count_to_second, on = .(CYCLER_ID, MOVEMENT)]
  sorted <- join_cap[order(CYCLER_ID, OTHER_MOVE, -target_strat)]

  sorted[, prio := seq_len(.N), by = .(CYCLER_ID, OTHER_MOVE)]
  max_prio <- sorted[, max(prio)]
  sorted[, PORB_MASS_LEFT := ifelse(prio == 1, 1, 0)]
  for (prio_loop in 1:max_prio) {

    sorted[, P_TO_FILL := ifelse(prio == prio_loop, PORB_MASS_LEFT, 0)]
    sorted[prio == prio_loop, P_MASS_FILLED := pmin(P_TO_FILL, mixed_cap, target_strat)]
    sorted[, PORB_MASS_LEFT := ifelse(prio == prio_loop + 1, 1 - sum(P_MASS_FILLED, na.rm = TRUE), 0), by = .(CYCLER_ID, OTHER_MOVE)]

  }
  sorted[, ':=' (top = NULL, bottom = NULL, PORB_MASS_LEFT = NULL, P_TO_FILL = NULL)]
  sorted[, LEFTOVER := 1 - sum(P_MASS_FILLED), by = .(CYCLER_ID, OTHER_MOVE)]
  sorted[, ROOM_TO_FILL := mixed_cap - P_MASS_FILLED]

  for (prio_loop in 2:max_prio) {
    sorted[, MAX_FILL_FOR_THIS_LOOP := min(ifelse(ROOM_TO_FILL == 0, 10000, ROOM_TO_FILL)), by = .(CYCLER_ID, OTHER_MOVE)]
    sorted[, FILLED_MOVEMENTS_LOOP_COUNT := sum(ifelse(ROOM_TO_FILL > 0, 1, 0)), by = .(CYCLER_ID, OTHER_MOVE)]
    sorted[ROOM_TO_FILL > 0, P_MASS_FILLED := P_MASS_FILLED + pmin(MAX_FILL_FOR_THIS_LOOP, LEFTOVER / FILLED_MOVEMENTS_LOOP_COUNT)]
    sorted[, LEFTOVER := 1 - sum(P_MASS_FILLED), by = .(CYCLER_ID, OTHER_MOVE)]
   # sorted[, FILL_AMOUNT_LOOP := ifelse(prio >= prio_loop, )
  }


  sorted

 # capped_strat <- constrained_mixed_strategy_long_target(join_card_count_to_second, static_model, first_cycler_per_team[, CYCLER_ID], other_move_vec2)
  #PURRKKAAA
  capped_strat <-  sorted[, .(CYCLER_ID, MOVEMENT, OTHER_MOVE, strategy = P_MASS_FILLED)]

  # capped_strat[, REMOVE_ME := if(uniqueN(strategy) == 1) TRUE else FALSE, by = .(CYCLER_ID, OTHER_MOVE)]
  # if (nrow(capped_strat[REMOVE_ME == TRUE])> 0) {
  #   warning("There might be a problem here. Previously REMOVE ME = FALSE was only included")
  #   print(capped_strat[REMOVE_ME == TRUE])
  #
  #   }
  # capped_strat <- capped_strat[REMOVE_ME == FALSE]

  join_back <- capped_strat[join_card_count_to_second, on = .(CYCLER_ID, MOVEMENT, OTHER_MOVE)]
  #let's fix NA with 0
  join_back[, strategy := ifelse(is.na(strategy),  0.00001, strategy)]
  ss_res_second <- join_back[, .(SECOND_CYCLER = CYCLER_ID, TEAM_ID, SECOND_MOVEMENT = MOVEMENT, SECOND_P = strategy, FIRST_MOVEMENT = OTHER_MOVE)]

  join_first_cyc <- ss_first_result[ss_res_second, on = .(TEAM_ID, FIRST_MOVEMENT)]

  join_first_cyc[, TOTAL_P := FIRST_P * SECOND_P]
  join_first_cyc[, comb_id := seq_len(.N)]
  rouler_ids <- ADM_CYCLER_INFO[CYCLER_TYPE_NAME == "Rouler", CYCLER_ID]
  sprint_ids <- ADM_CYCLER_INFO[CYCLER_TYPE_NAME == "Sprinteur", CYCLER_ID]
  roulers_player_first <- join_first_cyc[FIRST_CYCLER %in% rouler_ids][, .(M1 = FIRST_MOVEMENT,
                                                                           C1 = FIRST_CYCLER,
                                                                           PROB_ROULER = FIRST_P,
                                                                           TOTAL_P,
                                                                           TEAM_ID,
                                                                           comb_id)]
  rouler_played_second <- join_first_cyc[SECOND_CYCLER %in% rouler_ids][, .(M1 = SECOND_MOVEMENT,
                                                                            C1 = SECOND_CYCLER,
                                                                            PROB_ROULER = SECOND_P,
                                                                            TOTAL_P,
                                                                            TEAM_ID,
                                                                            comb_id)]
  sprinter_player_first <- join_first_cyc[FIRST_CYCLER %in% sprint_ids][, .(M2 = FIRST_MOVEMENT,
                                                                            C2 = FIRST_CYCLER,
                                                                            PROB_SPRINTER = FIRST_P,
                                                                            comb_id)]
  sprinter_played_second <- join_first_cyc[SECOND_CYCLER %in% sprint_ids][, .(M2 = SECOND_MOVEMENT,
                                                                              C2 = SECOND_CYCLER,
                                                                              PROB_SPRINTER = SECOND_P,
                                                                              comb_id)]
  append_roulers <- rbind(roulers_player_first, rouler_played_second)
  append_sprint <- rbind(sprinter_player_first, sprinter_played_second)
  join_data <- append_roulers[append_sprint, on = "comb_id"]
  combined_data <- join_data[, .(CYCLERS = paste0(C1, "_", C2),
                             MOVES = paste0(M1, "_", M2),
                             TEAM_ID,
                             new_prob = TOTAL_P,
                             C1, C2, M1, M2)]
  }
  append_with_one_cycler_team <- rbind(combined_data, one_cycler_team_result)

  append_with_one_cycler_team[, new_prob := ifelse(new_prob < 0.0001, 0.0001, new_prob)]

  join_new_strat <- append_with_one_cycler_team[combinations_data, on = .(CYCLERS, MOVES, TEAM_ID)]
 # join_new_strat[case_id == 1]
  # join_new_strat[, NEW_CASE_PROB := prod(new_prob)]
  temp <- join_new_strat[, .(TEAM_SCORE = sum(TOT_SCORE),

                                        MOVE_DIFF_SCORE  = sum(MOVE_DIFF_SCORE),
                                        EXHAUST_SCORE = sum(EXHAUST_SCORE),
                                        TTF_SCORE = sum(TTF_SCORE),
                                        TURNS_TO_FINISH = mean(TURNS_TO_FINISH),
                                    #    CYC_DIST_SCORE = sum(CYC_DIST_SCORE),
                                        MOVE_ORDER_SCORE = sum(MOVE_ORDER_SCORE),
                                        OVER_FINISH_SCORE = sum(OVER_FINISH_SCORE)),
                                    by = .(MOVES, CYCLERS, TEAM_ID, case_id, new_prob, C1, C2, M1, M2)]

  #temp[, WEIGHTED_SCORE := prod(new_prob) * TEAM_SCORE, by = case_id]
 # current_ev_data[case_id == 1]
  temp[, OPPONENT_CASE_PROB := prod(new_prob) / new_prob, by = case_id]

  temp[, WEIGHTED_SCORE := OPPONENT_CASE_PROB * TEAM_SCORE]

  result <- temp[, .(EV = sum(WEIGHTED_SCORE) / sum(OPPONENT_CASE_PROB),
                     MOVE_DIFF_SCORE  = sum(MOVE_DIFF_SCORE * OPPONENT_CASE_PROB) / sum(OPPONENT_CASE_PROB),
                     EXHAUST_SCORE = sum(EXHAUST_SCORE * OPPONENT_CASE_PROB) / sum(OPPONENT_CASE_PROB),
                     TTF_SCORE = sum(TTF_SCORE * OPPONENT_CASE_PROB) / sum(OPPONENT_CASE_PROB),
                     TURNS_TO_FINISH = sum(TURNS_TO_FINISH * OPPONENT_CASE_PROB) / sum(OPPONENT_CASE_PROB),
                 #    CYC_DIST_SCORE = sum(CYC_DIST_SCORE * OPPONENT_CASE_PROB),
                     MOVE_ORDER_SCORE = sum(MOVE_ORDER_SCORE * OPPONENT_CASE_PROB) / sum(OPPONENT_CASE_PROB),
                     OVER_FINISH_SCORE = sum(OVER_FINISH_SCORE * OPPONENT_CASE_PROB) / sum(OPPONENT_CASE_PROB) ), by = .(CYCLERS, MOVES, TEAM_ID, PROB_PRODUCT = new_prob, C1, C2, M1, M2)]

  return(result)


}
