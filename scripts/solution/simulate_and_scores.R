simulate_and_scores_phase_1 <- function(game_status, deck_status, team_id, STG_CYCLER, turn_id, ctM_data, cards_in_hands) {
  total_scores <-  NULL
    used_game_status <- copy(game_status)
  pre_res <- precalc_track(used_game_status)
  deck_copied <- copy(deck_status)
  orig_posits <- used_game_status[CYCLER_ID > 0, .(GAME_SLOT_ID, CYCLER_ID)]

  range_data <- calc_move_range(used_game_status, deck_status, ctM_data)
  #join team to range
  range_joined_team <- STG_CYCLER[range_data, on = "CYCLER_ID"]
  zoom(used_game_status)
  #my random card
  range_joined_team[, row_id := seq_len(.N)]

  for (simul_loop in 1:10) {

  my_move <- range_joined_team[TEAM_ID == team_id, .(row_id = sample(row_id, size = 1, prob = shared_odds))]
  moved_cycler <- range_joined_team[row_id == my_move[, row_id], CYCLER_ID]
  my_team_mate <- STG_CYCLER[TEAM_ID == team_id & CYCLER_ID != moved_cycler, CYCLER_ID ]
  move_amount <- range_joined_team[row_id == my_move[, row_id], MOVEMENT]
 # simulate_decision_v2 <- ss_teaM_phase_two[, .(row_id = sample(row_id, size = 1, prob = shared_odds)), by = TEAM_ID]
  simulated_position <- simulate_one_possibility(used_game_status,
                           deck_status,
                           STG_CYCLER,
                           turn_id,
                           moved_cycler,
                           move_amount,
                           ctM_data,
                           my_team_mate
                           )
  zoom(simulated_position$game_status,3, 20)

  range_data_2 <- calc_move_range_phase_2(simulated_position$game_status, deck_copied, ctM_data, simulated_position$p2_score)
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
  append_actions <- rbind(simul_phase2$played_cards[, phase := 2], simulated_position$moves_made[, phase := 1])
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


 res <-  score_position_light(simul_phase2$game_status, ADM_AI_CONF,  pre_res, ctM_data, orig_posits,
                       STG_CYCLER, append_actions)

 team_decision <- append_actions[phase == 1, .(CYCLER_ID, MOVEMENT)]
 join_team_to_dec <- STG_CYCLER[team_decision, on = "CYCLER_ID"]
#join_team
 join_team <- STG_CYCLER[res, on = "CYCLER_ID"]

  all_scores <- join_team[,.(Score = sum(Result)), by = .(TEAM_ID)][order(Score)]
  join_dec <- join_team_to_dec[all_scores, on = "TEAM_ID"]
total_scores <- rbind(join_dec, total_scores)
total_scores[, .(mean_score = mean(Score, na.rm = TRUE), .N), by = .(CYCLER_ID,TEAM_ID,MOVEMENT)][order(-mean_score)]
  }
 score_data <- total_scores[, .(mean_score = mean(Score, na.rm = TRUE), .N), by = .(CYCLER_ID,TEAM_ID,MOVEMENT)][order(-mean_score)]
  my_best_score <- score_data[TEAM_ID == team_id, max(mean_score)]
my_decision <- score_data[mean_score == my_best_score][1,. (CYCLER_ID, MOVEMENT)]
  return(my_decision)
}
