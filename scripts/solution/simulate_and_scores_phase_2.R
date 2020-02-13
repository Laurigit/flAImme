simulate_and_scores_phase_2 <- function(game_status, deck_status, cycler_id, STG_CYCLER, ctM_data, phase_one_actions, pre_res, card_options_in_hand) {
  #phase_two_cyclers <- c(1,2,3,4)
  total_scores <-  NULL
  used_game_status <- copy(game_status)

  deck_copied <- copy(deck_status)
  orig_posits <- used_game_status[CYCLER_ID > 0, .(GAME_SLOT_ID, CYCLER_ID)]

  phase_two_cyclers <- used_game_status[CYCLER_ID > 0 & !CYCLER_ID %in% phase_one_actions[, CYCLER_ID], CYCLER_ID]


  for (simul_loop in 1:10) {
    range_data <- calc_move_range(used_game_status, deck_status, ctM_data)
    p2ScoreOutout <- phase2_slot_score(used_game_status, phase_two_cyclers, deck_copied, range_data, pre_res)


    range_data_2 <- calc_move_range_phase_2(used_game_status, deck_copied, ctM_data, p2ScoreOutout)
    range_joined_team2 <- STG_CYCLER[range_data_2, on = "CYCLER_ID"]
    #my random card
    range_joined_team2[, row_id := seq_len(.N)]

    my_move <- range_joined_team2[CYCLER_ID == cycler_id & prio_group != 100 & MOVEMENT %in% card_options_in_hand, .(row_id = sample(row_id, size = 1, prob = shared_odds))]
    move_amount2 <- range_joined_team2[row_id == my_move[, row_id], MOVEMENT]

    simul_phase2 <- simulate_one_possibility_phase_2(used_game_status,
                                                     deck_status,
                                                     STG_CYCLER,

                                                     cycler_id,
                                                     move_amount2,
                                                     ctM_data,
                                                     phase_one_actions,
                                                     p2ScoreOutout)

    #check if we now turns_to finsih for each position
    posits <- simul_phase2$game_status[CYCLER_ID > 0, .(GAME_SLOT_ID, CYCLER_ID)]
    #append_all_played_cards

    append_actions <- rbind(simul_phase2$played_cards, phase_one_actions)
    #join played cards
    player_and_pos <-append_actions[posits, on = "CYCLER_ID"]
    #joinaa
    join_ctM <- ctM_data[player_and_pos, on = .(CYCLER_ID, MOVEMENT, new_slot_after_moving = GAME_SLOT_ID), .(new_slot_after_moving, CYCLER_ID, MOVEMENT, curr_pos, N = 0, turns_to_finish)][is.na(curr_pos)]
    #request new turns_to_finish
    if (nrow(join_ctM) > 0) {
      for (add_loop in 1:nrow(join_ctM)){

        loop_cycler <- join_ctM[add_loop, CYCLER_ID]
        cycler_deck_updated <- deck_copied[CYCLER_ID == loop_cycler]

        played_card <- join_ctM[add_loop, MOVEMENT]
        min_row_id_played <- cycler_deck_updated[Zone != "Removed", min(row_id)]
        cycler_deck_updated[row_id == min_row_id_played, Zone := "Removed"]


        new_slot <- join_ctM[add_loop, new_slot_after_moving]
        ft_res <- finish_turns_db(con, ADM_OPTIMAL_MOVES, simul_phase2$game_status, cycler_deck_updated, pre_res, slot)
        #print(ft_res$turns_to_finish)
        ADM_OPTIMAL_MOVES <- ft_res$new_ADM_OPT
        join_ctM[add_loop, turns_to_finish := ft_res$turns_to_finish]
        if (nrow(ctM_data[is.na(CYCLER_ID)]) > 1) {
          ctM_data <- ctM_data[!is.na(CYCLER_ID)]
          warning("STOPPING")
          stop()

        }

      }
    }
    #append to old ctm
    ctM_data <- rbind(ctM_data, join_ctM)[!is.na(turns_to_finish)]
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
