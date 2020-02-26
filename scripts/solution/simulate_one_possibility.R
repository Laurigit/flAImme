#phse_1
simulate_one_possibility <- function(game_status, deck_status, STG_CYCLER,  p1_cycler_id, p1_movement,
                                     ctM_data, p1_team_Mate, precalced_track, range_data) {

  take_copy_of_game <- copy(game_status)

  deck_copied <- copy(deck_status)
  not_played <- deck_copied[Zone != "Removed"]

  used_game_status <- copy(game_status)

  #this can be improved to use the known information when exhaust has been added
  # options <- not_played[, .N, by = .(MOVEMENT, CYCLER_ID)][order(CYCLER_ID)]
  # options[, turns_to_finish := 100]



 # range_data <- calc_move_range(take_copy_of_game, deck_status, ctM_data)
 # block_output <- blocking_odds_all(game_status, range_data, STG_CYCLER)
  #join blocki
  #join_blocked <- block_output[range_data, on = .(CYCLER_ID, MOVEMENT)]
 # join_blocked[, block_discount := ifelse(is.na(blocked_odds), 1, 1- blocked_odds)]
  #join team

  #ss_team[, block_adj_odds_first := splitted_odds * block_discount]
  #ss_team[, odds_grop_total_orig := sum(splitted_odds), by = .(TEAM_ID, turns_to_finish)]
  #ss_team[, odds_group_total := sum(block_adj_odds_first), by = .(TEAM_ID, turns_to_finish)]
  #ss_team[, final_block_adjusted_odds := block_adj_odds_first * odds_grop_total_orig / odds_group_total]

  simulate_decision <- range_data[, .(row_id = custom_sample(row_id, prob = splitted_odds)), by = TEAM_ID]
  simul_res <- range_data[simulate_decision, on = .(row_id, TEAM_ID)]
  sscols_simul_rs <- simul_res[, .(CYCLER_ID, MOVEMENT)]
#upate input_decision
  remove_input_cyclers <- sscols_simul_rs[!CYCLER_ID %in% c(p1_cycler_id, p1_team_Mate)]
  new_data <- data.table(CYCLER_ID = p1_cycler_id, MOVEMENT = p1_movement)
  bindaa <- rbind(remove_input_cyclers, new_data)


  phase_two_copy <- copy(take_copy_of_game)

  orderd_cyclers <- create_move_order_vec(phase_two_copy, bindaa[, CYCLER_ID])
  for (cycler_loop in orderd_cyclers) {
    phase_two_copy <- move_cycler(phase_two_copy, cycler_loop, bindaa[CYCLER_ID == cycler_loop, MOVEMENT])


  }


  #what I should do here is to calculate new move priority and their odds.
  phase_two_cyclers <- phase_two_copy[CYCLER_ID > 0 & !CYCLER_ID %in% bindaa[, CYCLER_ID], CYCLER_ID]

  p2ScoreOutout <- phase2_slot_score(phase_two_copy, phase_two_cyclers, deck_copied, range_data, precalced_track)
  res <- NULL
  res$game_status <- phase_two_copy
  res$moves_made <- bindaa
  res$p2_score <- p2ScoreOutout
  return(res)
}
