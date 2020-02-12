simulate_and_scores_phase_2 <- function(game_status, deck_status, team_id, STG_CYCLER, turn_id, ctM_data, phase_two_cyclers) {
  #phase_two_cyclers <- c(1,2,3,4)
  total_scores <-  NULL
  used_game_status <- copy(game_status)
  pre_res <- precalc_track(used_game_status)
  deck_copied <- copy(deck_status)
  orig_posits <- used_game_status[CYCLER_ID > 0, .(GAME_SLOT_ID, CYCLER_ID)]

  for (simul_loop in 1:10) {
    range_data <- calc_move_range(used_game_status, deck_status, ctM_data)
    p2ScoreOutout <- phase2_slot_score(used_game_status, phase_two_cyclers, deck_copied, range_data)


    range_data_2 <- calc_move_range_phase_2(used_game_status, deck_copied, ctM_data, simulated_position$p2_score)
    range_joined_team2 <- STG_CYCLER[range_data_2, on = "CYCLER_ID"]
    #my random card
    range_joined_team2[, row_id := seq_len(.N)]

    my_move <- range_joined_team2[TEAM_ID == team_id & prio_group != 100, .(row_id = sample(row_id, size = 1, prob = shared_odds))]
    moved_cycler2 <- range_joined_team2[row_id == my_move[, row_id], CYCLER_ID]
    move_amount2 <- range_joined_team2[row_id == my_move[, row_id], MOVEMENT]

    simul_phase2 <- simulate_one_possibility_phase_2(simulated_position$game_status,
                                                     deck_status,
                                                     STG_CYCLER,
                                                     turn_id,
                                                     moved_cycler2,
                                                     move_amount2,
                                                     ctM_data,
                                                     simulated_position$moves_made,
                                                     simulated_position$p2_score)

    #check if we now turns_to finsih for each position
    posits <- simul_phase2$game_status[CYCLER_ID > 0, .(GAME_SLOT_ID, CYCLER_ID)]
    #append_all_played_cards
    append_actions <-simul_phase2$played_cards
    #join played cards
    player_and_pos <-append_actions[posits, on = "CYCLER_ID"]
    #joinaa
    join_ctM <- ctM_data[player_and_pos, on = .(CYCLER_ID, MOVEMENT, new_slot_after_moving = GAME_SLOT_ID), .(new_slot_after_moving, CYCLER_ID, TURN_ID, MOVEMENT)][is.na(TURN_ID)]
    #request new turns_to_finish
    if (nrow(join_ctM) > 0) {
      for (add_loop in 1:nrow(join_ctM)){
        ctM_data <- cyclers_turns_MOVEMEMENT_combs(ctM_data, used_game_status, deck_status, turn_id, pre_res, c(join_ctM[add_loop, CYCLER_ID],
                                                                                                                join_ctM[add_loop, MOVEMENT],
                                                                                                                join_ctM[add_loop, new_slot_after_moving]))
        if (nrow(ctM_data[is.na(CYCLER_ID)]) > 1) {
          ctM_data <- ctM_data[!is.na(CYCLER_ID)]
          warning("STOPPING")
          stop()

        }

      }
    }

    all_cyclers <- simul_phase2$game_status[CYCLER_ID > 0, .(CYCLER_ID)]
    p2_cycler_actions <- simul_phase2$played_cards[all_cyclers, on = "CYCLER_ID"]
    p2_cycler_actions[is.na(MOVEMENT), MOVEMENT := 0]
    res <-  score_position_light(simul_phase2$game_status, ADM_AI_CONF,  pre_res, ctM_data, orig_posits,
                                 STG_CYCLER, p2_cycler_actions)
    #join_team_to_res
    res_team <- STG_CYCLER[res, on = "CYCLER_ID"]


    join_team_to_action <- STG_CYCLER[simul_phase2$played_cards, on = "CYCLER_ID"]
    all_scores <- res_team[,.(Score = sum(Result)), by = .(TEAM_ID)][order(Score)]

    #join action and score
    act_score <- join_team_to_action[all_scores, on = .(TEAM_ID)]

    total_scores <- rbind(act_score, total_scores)
    #total_scores[, .(mean_score = mean(Score, na.rm = TRUE), .N), by = .(CYCLER_ID,TEAM_ID,MOVEMENT)][order(-mean_score)]
  }
  score_data <- total_scores[, .(mean_score = mean(Score, na.rm = TRUE), .N), by = .(CYCLER_ID, TEAM_ID,MOVEMENT)][order(-mean_score)]
  my_best_score <- score_data[TEAM_ID == team_id, max(mean_score)]
  my_decision <- score_data[mean_score == my_best_score][1,. (CYCLER_ID, MOVEMENT)]
  return(my_decision)
}
