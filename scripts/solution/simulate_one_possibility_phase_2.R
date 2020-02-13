simulate_one_possibility_phase_2 <- function(game_status, deck_status, STG_CYCLER, p2_cycler_id, p2_movement,
                                             ctM_data, phase_one_cyclers, p2_score) {

  #second round simulation data
  #remove blocked options
  #remove options that are strictly worse. Same amount turns to finish, but no exhaust
  #if secured slipstream, then prefer that over noslip
  #simulate

  deck_copied <- copy(deck_status)
  phase_two_cyclers <- game_status[CYCLER_ID > 0 & !CYCLER_ID %in% phase_one_cyclers[, CYCLER_ID], CYCLER_ID]
  phase_two_copy <- copy(game_status)



  #run while blocking odds > 0.4
  max_odds <- 1

  while (max_odds  > 0.4) {

    range_data_2 <- calc_move_range_phase_2(phase_two_copy, deck_copied, ctM_data, p2_score)

    block_output_phase2 <- blocking_odds_all_phase2(phase_two_copy, range_data_2, STG_CYCLER, phase_two_cyclers)

    #remove blocked movements and calc again
    remove <- block_output_phase2[blocked_odds > 0.4]
    deck_copied <- remove[deck_copied, on = .(CYCLER_ID, MOVEMENT)]
    deck_copied <- deck_copied[is.na(blocked_odds),. (CYCLER_ID, MOVEMENT, CARD_ID, Zone, row_id)]
    max_odds <- remove[, max(blocked_odds)]
  }

  #jionteam
  ss_team_v2 <- STG_CYCLER[range_data_2, on = "CYCLER_ID"]
  ss_team_v2[, row_id := seq_len(.N)]
  #only phase two cyclers

  ss_teaM_phase_two <- ss_team_v2[!CYCLER_ID %in% c(p2_cycler_id) & CYCLER_ID %in% phase_two_cyclers & MOVEMENT > 0]
  simulate_decision_v2 <- ss_teaM_phase_two[, .(row_id = sample(row_id, size = 1, prob = shared_odds)), by = TEAM_ID]
  simul_res_v2 <- ss_team_v2[simulate_decision_v2, on = .(row_id)]

  sscols_simul_v2 <- simul_res_v2[, .(CYCLER_ID, MOVEMENT)]
  #upate input_decision

  new_data_v2 <- data.table(CYCLER_ID = p2_cycler_id, MOVEMENT = p2_movement)
  bindaa_v2 <- rbind(sscols_simul_v2, new_data_v2)

  for (cycler_loop in bindaa_v2[, CYCLER_ID]) {
    phase_two_copy <- move_cycler(phase_two_copy, cycler_loop, bindaa_v2[CYCLER_ID == cycler_loop, MOVEMENT])
  }
  zoom(phase_two_copy)
  #SLIPSTREAMMMMSA!!

  phase_two_copy <- apply_slipstream(phase_two_copy)
  res_vec <- NULL
  res_vec$game_status <- phase_two_copy
  res_vec$played_cards <- bindaa_v2


  return(res_vec)
}

