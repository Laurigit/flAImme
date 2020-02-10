select_move <- function(game_status, deck_status, cycler_id, STG_CYCLER, phase_two_cyclers = NULL, turn_id) {

  take_copy_of_game <- copy(game_status)

  deck_copied <- copy(deck_status)
  not_played <- deck_copied[Zone != "Removed"]

  used_game_status <- copy(game_status)

  #this can be improved to use the known information when exhaust has been added
  options <- not_played[, .N, by = .(MOVEMENT, CYCLER_ID)][order(CYCLER_ID)]
  options[, turns_to_finish := 100]
  pre_res <- precalc_track(used_game_status)
  ctM_data <- cyclers_turns_MOVEMEMENT_combs(ctM_data, game_status, deck_status, turn_id, pre_res, NULL)


  range_data <- calc_move_range(take_copy_of_game, deck_status, ctM_data)
  block_output <- blocking_odds_all(game_status, range_data, STG_CYCLER)
  #join blocki
  join_blocked <- block_output[range_data, on = .(CYCLER_ID, MOVEMENT)]
  join_blocked[, block_discount := ifelse(is.na(blocked_odds), 1, 1- blocked_odds)]
  #join team
  ss_team <- STG_CYCLER[join_blocked, on = "CYCLER_ID"]
  ss_team[, block_adj_odds_first := splitted_odds * block_discount]
  ss_team[, odds_grop_total_orig := sum(splitted_odds), by = .(TEAM_ID, turns_to_finish)]
  ss_team[, odds_group_total := sum(block_adj_odds_first), by = .(TEAM_ID, turns_to_finish)]
  ss_team[, final_block_adjusted_odds := block_adj_odds_first * odds_grop_total_orig / odds_group_total]
  ss_team[, row_id := seq_len(.N)]
  simulate_decision <- ss_team[, .(row_id = sample(row_id, size = 1, prob = final_block_adjusted_odds)), by = TEAM_ID]
  simul_res <- ss_team[simulate_decision, on = .(row_id)]

  phase_two_copy <- copy(take_copy_of_game)
  for (cycler_loop in simul_res[, CYCLER_ID]) {
    phase_two_copy <- move_cycler(phase_two_copy, cycler_loop, simul_res[CYCLER_ID == cycler_loop, MOVEMENT])


  }
  zoom(phase_two_copy)


  #second round simulation data
  #remove blocked options
  #remove options that are strictly worse. Same amount turns to finish, but no exhaust
  #if secured slipstream, then prefer that over noslip
  #simulate
  phase_two_cyclers <- game_status[CYCLER_ID > 0 & !CYCLER_ID %in% simul_res[, CYCLER_ID], CYCLER_ID]

 #what I should do here is to calculate new move priority and their odds.
  p2ScoreOutout <- phase2_slot_score(game_status, phase_two_cyclers, deck_copied, range_data)

  range_data_2 <- calc_move_range_phase_2(phase_two_copy, deck_copied, ctM_data, p2ScoreOutout)

  block_output_phase2 <- blocking_odds_all_phase2(phase_two_copy, range_data_2, STG_CYCLER, phase_two_cyclers)

  join_blocked_v2 <- block_output_phase2[range_data_2, on = .(CYCLER_ID, MOVEMENT)][CYCLER_ID %in% phase_two_cyclers]
  join_blocked_v2[, block_discount := ifelse(is.na(blocked_odds), 1, 1- blocked_odds)]
  #join team
  #jäit siihen, että ekana yritetään siirtää prion sisällä oddseja. Jos ei onnistu, niin pitäis siirtää alemmalle prio groupille. Ylös ei voi siirtää. Sitä varten laskettu cumsum_prios
  ss_team_v2 <- STG_CYCLER[join_blocked_v2, on = "CYCLER_ID"]
  ss_team_v2[, block_adj_odds_first := shared_odds * (block_discount ^ 2)]
  ss_team_v2[, odds_grop_total_orig := sum(shared_odds), by = .(TEAM_ID, prio_group)]
  ss_team_v2[, odds_group_total := sum(block_adj_odds_first), by = .(TEAM_ID, prio_group)]
  ss_team_v2[, final_block_adjusted_odds := block_adj_odds_first * odds_grop_total_orig / odds_group_total]
  ss_team_v2[, row_id := seq_len(.N)]
  simulate_decision_v2 <- ss_team_v2[, .(row_id = sample(row_id, size = 1, prob = final_block_adjusted_odds)), by = TEAM_ID]
  simul_res_v2 <- ss_team_v2[simulate_decision_v2, on = .(row_id)]

  for (cycler_loop in simul_res[, CYCLER_ID]) {
    phase_two_copy <- move_cycler(phase_two_copy, cycler_loop, simul_res[CYCLER_ID == cycler_loop, MOVEMENT])


  }
  zoom(phase_two_copy)




  move_options <- deck_status[Zone == "Hand" & CYCLER_ID == cycler_id, .N, by = MOVEMENT][, MOVEMENT]

  if (is.null(phase_two_cyclers)) {
    traffic_data <- traffic_estimator_all_data(game_status, range_data, STG_CYCLER, cycler_id, phase_two_cyclers = NULL)
  } else {
    traffic_data <- traffic_estimator_all_data(game_status, range_data, STG_CYCLER, cycler_id, phase_two_cyclers)
  }
#  move_options <- c(2,3,4)
#join traffic data
  blocking_penalty_moves <- traffic_data[MOVEMENT %in% move_options & sum_odds > 0.9 & !is.na(relevant_for_blocking)]

  result <- available_moves[1, MOVEMENT]
  return(result)
}
