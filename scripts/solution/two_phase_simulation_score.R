two_phase_simulation_score <- function(game_status,
                                       deck_status,
                                       team_id,
                                       STG_CYCLER,
                                       turn_id,
                                       ctM_data,
                                       pre_res,
                                       range_joined_team,
                                       card_options = NULL,
                                       cycler_id = NULL,
                                       phase_one_actions = NULL,
                                       simul_rounds = 10,
                                       simulate_until_stopped = FALSE,
                                       ADM_AI_CONF) {
  total_scores <-  NULL
  used_game_status <- copy(game_status)
  deck_copied <- copy(deck_status)
  team_cyclers <- STG_CYCLER[TEAM_ID == team_id, CYCLER_ID]
  orig_posits <- used_game_status[CYCLER_ID > 0, .(GAME_SLOT_ID, CYCLER_ID)]
  #simul_rounds <- 10




  #
  if(!is.null(phase_one_actions)) {
    p2_game_status <- used_game_status
    phase_two_cyclers <- p2_game_status[!CYCLER_ID %in% phase_one_actions[, CYCLER_ID], CYCLER_ID]
    phase_one_cyclers <- p2_game_status[CYCLER_ID %in% phase_one_actions[, CYCLER_ID], CYCLER_ID]
    p2_score_outout <- phase2_slot_score(p2_game_status, phase_two_cyclers, deck_copied, range_joined_team, pre_res)
    range_data_2 <- calc_move_range_phase_2(p2_game_status, deck_copied, ctM_data, p2_score_outout)
    range_joined_team2 <- STG_CYCLER[range_data_2, on = "CYCLER_ID"]
    range_joined_team2[, row_id := seq_len(.N)]
    moves_made_phase_1 <- phase_one_actions

  }



  for (simul_loop in 1:simul_rounds) {
    # browser()
    # httpuv:::service()
    # if (isTRUE(session$input$stopThis)) {
    #   break()
    # }


        if (is.null(phase_one_actions)) {
      if (is.null(card_options)) {
        my_move <- range_joined_team[TEAM_ID == team_id, .(row_id = custom_sample(row_id, prob = splitted_odds))]
      } else {
        my_move <- range_joined_team[TEAM_ID == team_id & CYCLER_ID == cycler_id & MOVEMENT %in% card_options,
                                     .(row_id = custom_sample(row_id, prob = splitted_odds))]
      }

      moved_cycler <- range_joined_team[row_id == my_move[, row_id], CYCLER_ID]
      my_team_mate <- STG_CYCLER[TEAM_ID == team_id & CYCLER_ID != moved_cycler, CYCLER_ID ]
      move_amount <- range_joined_team[row_id == my_move[, row_id], MOVEMENT]
      # simulate_decision_v2 <- ss_teaM_phase_two[, .(row_id = sample(row_id, size = 1, prob = shared_odds)), by = TEAM_ID]
      simulated_position <- simulate_one_possibility(used_game_status,
                                                     deck_status,
                                                     STG_CYCLER,

                                                     moved_cycler,
                                                     move_amount,
                                                     ctM_data,
                                                     my_team_mate,
                                                     pre_res,
                                                     range_joined_team

      )
      moves_made_phase_1 <- simulated_position$moves_made
      moves_made_phase_1[, phase := 1]
      # zoom(simulated_position$game_status,3, 20)
      p2_score_outout <- simulated_position$p2_score
      p2_game_status <- simulated_position$game_status
      range_data_2 <- calc_move_range_phase_2(p2_game_status, deck_copied, ctM_data, p2_score_outout)
      range_joined_team2 <- STG_CYCLER[range_data_2, on = "CYCLER_ID"]
      #my random card
      range_joined_team2[, row_id := seq_len(.N)]
      # if (nrow(range_joined_team2[TEAM_ID == team_id & prio_group != 100]) == 0) {
      #   browser()
      # }

    # } else{
    #   #we are already on phase two!
    #   #calc p2 scores for only once
    #   p2_game_status <- used_game_status
    #   phase_two_cyclers <- p2_game_status[!CYCLER_ID %in% phase_one_actions[, CYCLER_ID], CYCLER_ID]
    #   phase_one_cyclers <- p2_game_status[CYCLER_ID %in% phase_one_actions[, CYCLER_ID], CYCLER_ID]
    #   p2_score_outout <- phase2_slot_score(p2_game_status, phase_two_cyclers, deck_copied, range_joined_team, pre_res)
    #   range_data_2 <- calc_move_range_phase_2(p2_game_status, deck_copied, ctM_data, p2_score_outout)
    #   range_joined_team2 <- STG_CYCLER[range_data_2, on = "CYCLER_ID"]
    #   range_joined_team2[, row_id := seq_len(.N)]
    #   moves_made_phase_1 <- phase_one_actions

      }

    #if in case only one or zero cycler left



    if (is.null(card_options) | is.null(phase_one_actions)) {
      my_move <- range_joined_team2[TEAM_ID == team_id & prio_group != 100, .(row_id = custom_sample(row_id, prob = odds))]
    } else {

      my_move <- range_joined_team2[CYCLER_ID == cycler_id & prio_group != 100 & MOVEMENT %in% card_options,
                                    .(row_id = custom_sample(row_id, prob = odds))]
    }





    moved_cycler2 <- range_joined_team2[row_id == my_move[, row_id], CYCLER_ID]
    move_amount2 <- range_joined_team2[row_id == my_move[, row_id], MOVEMENT]

    simul_phase2 <- simulate_one_possibility_phase_2(p2_game_status,
                                                     deck_status,
                                                     STG_CYCLER,
                                                     moved_cycler2,
                                                     move_amount2,
                                                     ctM_data,
                                                     moves_made_phase_1,
                                                     range_data_2)

    #check if we now turns_to finsih for each position
    posits <- simul_phase2$game_status[CYCLER_ID > 0, .(GAME_SLOT_ID, CYCLER_ID)]
    #append_all_played_cards
    simul_phase2$played_cards[, phase := 2]

    append_actions <- rbind(simul_phase2$played_cards,  moves_made_phase_1)
    #join played cards

    player_and_pos <- append_actions[posits, on = "CYCLER_ID"]

    #join_draw_odds

#lets calculate draw odds for everyone, but lets not use the information. Only use own cyclers
    player_and_pos[, DRAW_ODDS :=  calculate_draw_distribution_by_turn(CYCLER_ID,
                                                                                                           play_card(CYCLER_ID,
                                                                                                                     card_id = NULL,
                                                                                                                     deck_copied,
                                                                                                                     game_id = 0,
                                                                                                                     turn_id = 0,
                                                                                                                     con = FALSE,
                                                                                                                     card_row_id = NULL,
                                                                                                                     MOVEMENT_PLAYED = MOVEMENT),
                                                                                                           4, db_res = TRUE), by = CYCLER_ID]

    #removed exrtra info
    player_and_pos[!CYCLER_ID %in% team_cyclers, DRAW_ODDS := ""]
    #joinaa
    join_ctM <- ctM_data[player_and_pos, on = .(CYCLER_ID, MOVEMENT, new_slot_after_moving = GAME_SLOT_ID, DRAW_ODDS), .(DRAW_ODDS, new_slot_after_moving, CYCLER_ID, MOVEMENT, curr_pos, actual_movement, N = 0, TURNS_TO_FINISH)][is.na(curr_pos)]




    #request new TURNS_TO_FINISH
    if (nrow(join_ctM) > 0) {
      for (add_loop in 1:nrow(join_ctM)){
        loop_cycler <- join_ctM[add_loop, CYCLER_ID]
        cycler_deck_updated <- deck_copied[CYCLER_ID == loop_cycler]

        # played_card <- join_ctM[add_loop, MOVEMENT]
        #
        # min_row_id_played <- cycler_deck_updated[Zone != "Removed" & MOVEMENT == played_card, min(row_id)]
        # cycler_deck_updated[row_id == min_row_id_played, Zone := "Removed"]
        # cycler_deck_updated[Zone == "Hand", Zone := "Recycled"]
        draw_odds_raw_data <- join_ctM[add_loop, DRAW_ODDS]



        new_slot <- join_ctM[add_loop, new_slot_after_moving]
        ft_res <- finish_turns_db(con, ADM_OPTIMAL_MOVES, simul_phase2$game_status, cycler_deck_updated, pre_res, new_slot, draw_odds_raw_data)
        #print(ft_res$TURNS_TO_FINISH)
        ADM_OPTIMAL_MOVES <<- ft_res$new_ADM_OPT
        join_ctM[add_loop, TURNS_TO_FINISH := ft_res$TURNS_TO_FINISH]
        join_ctM[, curr_pos := ifelse(is.na(curr_pos), -100, curr_pos)]
        if (nrow(ctM_data[is.na(CYCLER_ID)]) > 1) {
          ctM_data <- ctM_data[!is.na(CYCLER_ID)]
          warning("STOPPING")
          stop()

        }

      }
    }
    #append to old ctm
    ctM_data <- rbind(ctM_data, join_ctM)[!is.na(TURNS_TO_FINISH)]
#
#     #in which slot am I? What is left in my deck?
#     temp_stat <- play_card(3, 3, deck_status[CYCLER_ID == 3], 1, 1)
#     draw_odds <- calculate_draw_distribution_by_turn(3, temp_stat, 4)
#     updated_deck <-

    res <-  score_position_light(simul_phase2$game_status, ADM_AI_CONF,  pre_res, ctM_data, orig_posits,
                                 STG_CYCLER, append_actions)

    res[, simul_round := simul_loop]

    if (!exists("total_simul_res")) {
      # res[, avain := "KEY"]
      # dummy_table <- data.table(avain = "KEY", simul_rounds = 1: simul_rounds)
      # simul_round_vect <- dummy_table[res, on = "avain", allow.cartesian = TRUE]
      # simul_round_vect_sort <- simul_round_vect[order(simul_round, res_rows)]
      total_simul_res <- res
    } else {
      total_simul_res <- rbind(total_simul_res, res)
    }

  }
    result <- NULL
    result$scores <- total_simul_res
    result$updated_ctm <- ctM_data
  return(result)
}
