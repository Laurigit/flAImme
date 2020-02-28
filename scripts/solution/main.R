  # required_data(c("STG_CYCLER", "STG_TRACK"))
kaadu
input_STARTUP_DATA <- data.table(CYCLER_ID = c(1,2,3,4,5,6,7,8,9,10,11,12),
                                   PLAYER_ID = c(1,1,2,2,3,3,4,4,5,5,6,6),
                                   exhaust = c(0, 0, 0, 0, 0, 0,0,0,0,0,0,0),
                                   starting_row =   c(1, 1, 1, 2, 2, 2,3,3,3,4,4,4),
                                    starting_lane = c(1,2, 3, 1, 2, 3,1,2,3,1,2,3))
   track <- 14
   # input_STARTUP_DATA <- data.table(CYCLER_ID = c(1,2,3,4),
   #                                  PLAYER_ID = c(1,1,2,2),
   #                                  exhaust = c(0, 0, 0, 0),
   #                                  starting_row =   c(1, 1, 2, 2),
   #                                  starting_lane = c(1,2, 1, 2))
   # track <- 3

 cycler_player_dt <- data.table(CYCLER_ID = c(1,2,3,4,5,6,7,8,9,10,11,12), PLAYER_ID = c(1,1,2,2,3,3,4,4,5,5,6,6))
 lane_data <-    data.table(  exhaust = c(0, 0, 0, 0, 0, 0,0,0,0,0,0,0),
                                  starting_row =   c(1, 1, 1, 2, 2, 2,3,3,3,4,4,4),
                              starting_lane = c(1,2, 3, 1, 2, 3,1,2,3,1,2,3))
con <- connDB(con, "flaimme")
   required_data(c("ADM_CYCLER_DECK", "ADM_OPTIMAL_MOVES", "STG_TRACK", "SRC_TRACK", "SRC_TRACK_PIECE", "STG_TRACK_PIECE", "SRC_AI_CONF", "STG_AI_CONF", "ADM_AI_CONF"), force_update =TRUE)
  total_winner <- NULL


  full_action <- NULL
  game_action <- NULL
  for (game_id in 1:200000) {
    track <- as.integer(runif(1, 12, 17))
    winner_state <- data.table(TURN_ID = numeric(), CYCLER_ID = numeric(), POSITION = numeric(), GAME_ID = numeric(),
                               row_over_finish = numeric(), finish_square = numeric())

    if (is.null(total_winner)) {
      used_startup_data <- input_STARTUP_DATA
    } else{
      stats <- total_winner[, mean(POSITION), by = CYCLER_ID][sample(total_winner[,unique(CYCLER_ID)])]
      stats[, rank := seq_len(.N)]
      join_stats <- stats[cycler_player_dt, on = "CYCLER_ID"][order(-rank)]
      cbindaa <- cbind(join_stats, lane_data)
      sscols_startup <- cbindaa[, .(CYCLER_ID, PLAYER_ID, exhaust, starting_lane, starting_row)]
      used_startup_data <- sscols_startup
    }



    game_status <- start_game(used_startup_data,track, STG_TRACK_PIECE, STG_TRACK)
    #add_mountain_inf
    game_status <- slots_out_of_mountains(game_status)
    game_status <- slots_out_of_mountains_to_track(game_status)
    orig_posits <- game_status[CYCLER_ID > 0, .(GAME_SLOT_ID, PIECE_ATTRIBUTE), by = CYCLER_ID]

    #initial decks
    deck_status <- create_decks(input_STARTUP_DATA[, CYCLER_ID], ADM_CYCLER_DECK)

   # required_data(c("ADM_AI_CONF"), force_update =TRUE)
    cyclers <- input_STARTUP_DATA[, CYCLER_ID]
    smart_cyclers <- c(3, 4)
    action_data <- data.table(CYCLER_ID = 1, EXHAUST = 1, GAME_SLOT_ID = 1,
                              BLOCK_DIFF = 1,
                              ASCEND_GAIN = 1,
                              SLIP_BONUS = 1,
                              TOTAL_MOVEMENT = 1, TURN_ID = 1)[1 == 0]
    #draw phase
    turn_amount <- 25
    move_data <- data.table(CYCLER_ID = cyclers, MOVEMENT = 0, CARD_ID = 0, key_col = "1")
    played_cards_data <- merge(x = data.table(TURN_ID = 1:turn_amount, key_col = "1"), y = move_data, by = "key_col", all = TRUE, allow.cartesian = TRUE)
    played_cards_data[, key_col := NULL]
    played_cards_data[, PHASE := 0]
    phase_data <- data.table(CYCLER_ID = cyclers, phase = cyclers %% 2 + 1 )
    new_EV <- NULL

    finish <- game_status[FINISH == 1, max(GAME_SLOT_ID)]

    for(turn_id in 1:turn_amount) {

      orig_posits <- game_status[CYCLER_ID > 0, .(GAME_SLOT_ID, PIECE_ATTRIBUTE), by = CYCLER_ID]

      #print(turn_id)

      #everybody draws
      for(loop in cyclers) {
        deck_status <- draw_cards(loop, deck_status)
      }



      #spirters move first
      #split cyclers to two phases


      if (!exists("ctM_data")) {
        ctM_data <- NULL
      }
      pre_aggr_game_status <- precalc_track(game_status )

      #ADM_OPTIMAL_MOVES <- data.table(DECK_LEFT = "9993322", TRACK_LEFT = "NNNNNNNNNNNMMMAANNNNNNNNNN", TURNS_TO_FINISH = 3)
      ctM_res <- cyclers_turns_MOVEMEMENT_combs(con, ADM_OPTIMAL_MOVES, game_status, deck_status, pre_aggr_game_status)
      ADM_OPTIMAL_MOVES <- ctM_res$new_ADM_OPT
      ctM_data <- ctM_res$ctM_data

      if(is.null(new_EV)) {
        new_EV <- ctM_data[, .(Best = min(turns_to_finish), EV = sum(turns_to_finish * N) / sum(N)), by = CYCLER_ID][order(EV)]
      }
      prev_EV <- new_EV[, .(Best_old = Best, EV_old = EV, CYCLER_ID)]
      new_EV <- ctM_data[, .(Best = min(turns_to_finish), EV = sum(turns_to_finish * N) / sum(N)), by = CYCLER_ID][order(EV)]
      joinaa <- prev_EV[new_EV, on = "CYCLER_ID"]


      Exha_count <- deck_status[Zone != "Removed" & MOVEMENT == 2, .(Exhaust = .N), by = CYCLER_ID]
      Mean_move <- deck_status[Zone != "Removed" & MOVEMENT > 2 , .(SUM_POWER_MOVE = sum(MOVEMENT), MAX_MOVE = max(MOVEMENT)), by = CYCLER_ID]
      join_ex_move <- Exha_count[Mean_move, on = "CYCLER_ID"]
      join_tot_stat <- join_ex_move[joinaa, on = "CYCLER_ID"]
      action_data_aggr <- action_data[, .(EXHAUST = sum(EXHAUST), BLOCK_DIFF = sum(BLOCK_DIFF),
                                                        ASCEND_GAIN = sum(ASCEND_GAIN),
                                                        SLIP_BONUS = sum(SLIP_BONUS)), by = CYCLER_ID]
      join_stat_and_aggr <- join_tot_stat[action_data_aggr, on = "CYCLER_ID"]
      track_left_vs_deck <- orig_posits[join_stat_and_aggr, on = "CYCLER_ID"]
      track_left_vs_deck[, MISSING_MOVES := finish - GAME_SLOT_ID - SUM_POWER_MOVE]

      sscols_status <- track_left_vs_deck[, .(CYCLER_ID, Best, EV, MISSING_MOVES, BLOCK_DIFF, ASCEND_GAIN, SLIP_BONUS, EXHAUST, EXH_LEFT = Exhaust, MAX_MOVE, SUM_POWER_MOVE)][order(Best)]
      #print(join_tot_stat)

      for (phase_loop in 1:2) {

        phase_cyclers <- phase_data[phase == phase_loop & CYCLER_ID %in% cyclers, CYCLER_ID]
        ai_cyclers <-  setdiff(phase_cyclers, smart_cyclers)


       # pelatut_kortit <- deck_status[sample(nrow(deck_status))][Zone == "Hand", .SD[1:1], .(CYCLER_ID)][, .(CYCLER_ID, CARD_ID, MOVEMENT)][CYCLER_ID %in% cyclers]
      for(loop in ai_cyclers) {
        move_selected  <-  card_selector_by_stat(game_status, deck_status[CYCLER_ID == loop & Zone == "Hand"], loop, "SMART_MAX",  aim_downhill = TRUE)[, MOVEMENT]
        card_id <- deck_status[CYCLER_ID == loop & MOVEMENT == move_selected, min(CARD_ID)]
        played_cards_data[TURN_ID == turn_id & CYCLER_ID == loop,  CARD_ID := card_id]
        played_cards_data[TURN_ID == turn_id & CYCLER_ID == loop, MOVEMENT := move_selected]
        played_cards_data[TURN_ID == turn_id & CYCLER_ID == loop, PHASE := phase_loop]
      }

      #action_data <- data.table(CYCLER_ID = c(1,2,3,4,5,6), MOVEMENT = c(2,3,2,5,4,3), move_order = c(1,2,3,4,5,6), TEAM_ID = c(1,1,2,2,3,3))



          action_data3 <- played_cards_data[TURN_ID == turn_id,. (CYCLER_ID, 0)]
          CYCLER_ORDER <- game_status[CYCLER_ID > 0][order(-SQUARE_ID)][, .(CYCLER_ID, move_order = seq_len(.N))]
          action_data2 <- CYCLER_ORDER[action_data3, on = "CYCLER_ID"]
          ss_team <- STG_CYCLER[, .(CYCLER_ID, TEAM_ID)]
          action_data1 <- ss_team[action_data2, on = "CYCLER_ID"]
          #cycler 1 hand



          smart_phase_cycler <- intersect(smart_cyclers, cyclers)
          team_id <- 2

          print(sscols_status)
          #print(sscols_status[CYCLER_ID %in% c(3,4)][order(CYCLER_ID)])

          #if anyone is finished, stop calc and to smart max
          if (nrow(winner_state) > 0) {
            turn_cycler <- played_cards_data[CYCLER_ID %in% smart_phase_cycler & TURN_ID == turn_id & MOVEMENT == 0, CYCLER_ID]
            if (length(turn_cycler) > 0) {
              for (loop in turn_cycler) {
                if (phase_loop == 1){
                move_selected  <-  card_selector_by_stat(game_status, deck_status[CYCLER_ID == loop & Zone == "Hand"], loop, "SMART_MAX",  aim_downhill = TRUE)[, MOVEMENT]
                card_id <- deck_status[CYCLER_ID == loop & MOVEMENT == move_selected, min(CARD_ID)]
                played_cards_data[TURN_ID == turn_id & CYCLER_ID == loop,  CARD_ID := card_id]
                played_cards_data[TURN_ID == turn_id & CYCLER_ID == loop, MOVEMENT := move_selected]
                played_cards_data[TURN_ID == turn_id & CYCLER_ID == loop, PHASE := phase_loop]
                }
              }
            }

          } else {


            if (phase_loop == 1) {
              #calculate move range before phase 1 actions
              range_joined_team <- calc_move_range(game_status, deck_status, ctM_data, STG_CYCLER)
              print(range_joined_team[CYCLER_ID %in% smart_cyclers])


              simult_list_res <-  two_phase_simulation_score(game_status, deck_status, team_id, STG_CYCLER, turn_id,
                                                             ctM_data, pre_aggr_game_status, range_joined_team,
                                                             card_options = NULL, cycler_id = NULL, phase_one_actions = NULL,
                                                             simul_rounds = 5,
                                                             simulate_until_stopped = FALSE,
                                                             ADM_AI_CONF = ADM_AI_CONF)
              ctM_data <- simult_list_res$updated_ctm
              move_first_cycler <- which_cycler_to_move_first(simult_list_res$scores, STG_CYCLER, team_id)

             # simulation_data_from_which_cycler <- move_first_cycler_result$simulation
              #look at my cards


              card_options_in_hand <- smart_cards_options(deck_status[CYCLER_ID == move_first_cycler & Zone == "Hand", unique(MOVEMENT)], pre_aggr_game_status, move_first_cycler)
              if (length(card_options_in_hand) == 1) {
                move_amount <- card_options_in_hand
              } else {


             # print(zoom(game_status))
              phase_1_simul <-  two_phase_simulation_score(game_status, deck_status, team_id, STG_CYCLER, turn_id, ctM_data, pre_aggr_game_status,
                                                           range_joined_team,
                                                           card_options = card_options_in_hand, cycler_id = move_first_cycler,
                                                           phase_one_actions = NULL,
                                                           simul_rounds = 15,
                                                           simulate_until_stopped = FALSE,
                                                           ADM_AI_CONF = ADM_AI_CONF)

              team_scores <- phase_1_simul$scores[CYCLER_ID %in% smart_cyclers]
              agg_score <- team_scores[Result != 0, .(Result = mean(Result)), by = .(CYCLER_ID, MOVEMENT, Setting, phase)][order(CYCLER_ID, MOVEMENT)]
              print(dcast.data.table(agg_score, formula = CYCLER_ID + Setting ~ MOVEMENT, value.var = "Result"))
              ctM_data <- phase_1_simul$updated_ctm
        #  simul_res_p1 <-  simulate_and_scores_phase_1(phase_1_simul$scores, STG_CYCLER, move_first_cycler)

              simul_res_p1 <-  simulate_and_scores_phase_2(phase_1_simul, STG_CYCLER, team_id, move_first_cycler)
            move_amount <-  simul_res_p1[, MOVEMENT]
              }
              move_cyc <- move_first_cycler

              card_id <- deck_status[CYCLER_ID == move_cyc & MOVEMENT == move_amount, min(CARD_ID)]
            played_cards_data[CYCLER_ID  == move_first_cycler & TURN_ID == turn_id, MOVEMENT := move_amount]
            played_cards_data[CYCLER_ID  == move_first_cycler & TURN_ID == turn_id, PHASE := phase_loop]
            played_cards_data[TURN_ID == turn_id & CYCLER_ID == move_first_cycler, CARD_ID := card_id]
            print(paste0("Moved cycler ", move_first_cycler, ". Options: ", paste0(card_options_in_hand, collapse = " "),
                         " Played card: ", move_amount))
            } else if (phase_loop == 2) {



              #who am i
              second_cycler <- STG_CYCLER[TEAM_ID == STG_CYCLER[CYCLER_ID == move_first_cycler, TEAM_ID] & CYCLER_ID != move_first_cycler, CYCLER_ID]

                card_options_in_hand_p2 <- smart_cards_options(deck_status[CYCLER_ID == second_cycler & Zone == "Hand", unique(MOVEMENT)], pre_aggr_game_status, second_cycler)
                if (length(card_options_in_hand_p2) == 1) {
                  move_amount_p2 <- card_options_in_hand_p2
                } else {






              phase_one_actions <-  played_cards_data[TURN_ID == turn_id & MOVEMENT & PHASE == 1,. (CYCLER_ID, MOVEMENT, phase = PHASE)]

              phase_2_simul <-  two_phase_simulation_score(game_status, deck_status, team_id, STG_CYCLER, turn_id, ctM_data, pre_aggr_game_status,
                                                           range_joined_team,
                                                           card_options = card_options_in_hand_p2, cycler_id = second_cycler,
                                                           phase_one_actions = phase_one_actions,
                                                           simul_rounds = 30,
                                                           simulate_until_stopped = FALSE,
                                                           ADM_AI_CONF = ADM_AI_CONF)
              aggr2 <- phase_2_simul$scores[CYCLER_ID == second_cycler & Result != 0, .(Result = mean(Result)), by = .(MOVEMENT, Setting)]

              #print(aggr2)
              print(second_cycler)
              print(dcast.data.table(aggr2, formula =  Setting ~ MOVEMENT, value.var = "Result"))

            simul_rs_p2 <-  simulate_and_scores_phase_2(phase_2_simul, STG_CYCLER, team_id, second_cycler)
            move_amount_p2 <-  simul_rs_p2[, MOVEMENT]
                }
            move_cyc <- simul_rs_p2[, CYCLER_ID]


            print(paste0("Moved 2nd cycler ", second_cycler, ". Options: ", paste0(card_options_in_hand_p2, collapse = " "),
                         " Played card: ", move_amount_p2))

            card_id_p2 <- deck_status[CYCLER_ID == move_cyc & MOVEMENT == move_amount_p2, min(CARD_ID)]
            played_cards_data[CYCLER_ID  == move_cyc & TURN_ID == turn_id, MOVEMENT := move_amount_p2]
            played_cards_data[CYCLER_ID  == move_cyc & TURN_ID == turn_id, PHASE := phase_loop]
            played_cards_data[TURN_ID == turn_id & CYCLER_ID == move_cyc, CARD_ID := card_id_p2]
              }

          }


          phase_cyclers <- played_cards_data[TURN_ID == turn_id & PHASE == phase_loop, CYCLER_ID]



          phase_cyclers_in_move_order <- create_move_order_vec(game_status, phase_cyclers)

          print(zoom(game_status))
        #  browser()
      for(loop_move in phase_cyclers_in_move_order) {

        row_data <- played_cards_data[TURN_ID == turn_id & loop_move == CYCLER_ID & PHASE == phase_loop]

        deck_status <- play_card(cycler_id = loop_move,
                                 card_id = row_data[, CARD_ID],
                                 current_decks = deck_status, 1, 1, FALSE)
        game_status <- move_cycler(game_status, loop_move, movement = row_data[, MOVEMENT])

      }

          new_posits_phase <- game_status[CYCLER_ID > 0, .(GAME_SLOT_ID_NEW = GAME_SLOT_ID), by = CYCLER_ID]
          diff_phase <- orig_posits[new_posits_phase, on = "CYCLER_ID"][, ACTUAL_MOVEMENT := GAME_SLOT_ID_NEW - GAME_SLOT_ID]
          phase_2_cyclers <- diff_phase[ACTUAL_MOVEMENT == 0, CYCLER_ID]
      } #end of phase
      new_posits <- game_status[CYCLER_ID > 0, .(GAME_SLOT_ID_NEW = GAME_SLOT_ID), by = CYCLER_ID]
      diff <- orig_posits[new_posits, on = "CYCLER_ID"][, ACTUAL_MOVEMENT := GAME_SLOT_ID_NEW - GAME_SLOT_ID]


      game_status <- apply_slipstream(game_status)

     # print(paste0("TURN DONE ", turn_id))

  #  print(deck_status[CYCLER_ID == 3])
      slip_status <- game_status[CYCLER_ID > 0, .(GAME_SLOT_SLIP = GAME_SLOT_ID), by = CYCLER_ID]
      diff_slip <- diff[slip_status, on = "CYCLER_ID"][, ':=' (SLIP_BONUS = GAME_SLOT_SLIP - GAME_SLOT_ID_NEW,
                                                               TOTAL_MOVEMENT = GAME_SLOT_SLIP - GAME_SLOT_ID)]
      movementit <- played_cards_data[TURN_ID == turn_id,. (CYCLER_ID, MOVEMENT, PHASE)]

      join_exh <- calc_exhaust(game_status)[diff_slip, on = "CYCLER_ID"]
      join_movement <- join_exh[movementit, on = "CYCLER_ID"]
      action_row <- join_movement[, .(CYCLER_ID, EXHAUST, GAME_SLOT_ID = GAME_SLOT_SLIP,
                                      BLOCK_DIFF = pmin(ACTUAL_MOVEMENT - MOVEMENT, 0),
                                      ASCEND_GAIN = pmax(ACTUAL_MOVEMENT - MOVEMENT, 0),
                                      SLIP_BONUS,
                                      TOTAL_MOVEMENT, TURN_ID = turn_id)]


      deck_status <- apply_exhaustion(deck_status, game_status)


      #deck stats
     # deck_stats <- dcast.data.table(deck_status[Zone != "Removed", .(clist = list(CARD_ID)), by = .(CYCLER_ID, Zone)],
       #                             formula = CYCLER_ID ~ Zone, value.var = "clist")

      winner_state <- check_winner(game_status, winner_state, turn_id, game_id)

      cyclers <- setdiff(cyclers, winner_state[, CYCLER_ID])
      game_status <- clear_finishers(game_status,winner_state[, CYCLER_ID])
      #game_status[CYCLER_ID > 0][order(-GAME_SLOT_ID)]
      #deck_status[, .N, by = .(CYCLER_ID, Zone)][order(Zone, CYCLER_ID)]
     # deck_left <- deck_status[Zone != "Removed", .(Mean_Mov = mean(MOVEMENT)), by = CYCLER_ID]
     # deck_power <- deck_status[Zone != "Removed" & MOVEMENT > 2, .(Mean_power_mov = mean(MOVEMENT)), by = CYCLER_ID]
      #join_left_and_power <- deck_power[deck_left, on = "CYCLER_ID"]
      action_data <- rbind(action_data, action_row, fill = TRUE)

      #print(action_row)
      #print(ctM_data[CYCLER_ID %in% smart_cyclers])
      #action_data <- join_left_and_power[action_row, on = "CYCLER_ID"]
      #aggr_action <- action_data[, .(Mean_power_mov = mean(Mean_power_mov, na.rm = TRUE),
      #                               Mean_Mov = mean(Mean_Mov, na.rm = TRUE),
      #                               SLIP = sum(SLIP_BONUS, na.rm = TRUE), BLOCK = sum(BLOCK_DIFF, na.rm = TRUE),
      #                               EXHAUST = sum(EXHAUST, na.rm = TRUE), ASCEND = sum(ASCEND_GAIN, na.rm = TRUE)),
      #                           by = CYCLER_ID][order(CYCLER_ID)][, GAME_ID := game_id]
      #print(zoom(game_status, 8, 7))
      # time_now <- Sys.time()
      # tdiff <- prev_time -
      #deck_status[CYCLER_ID == 1]
      if (length(cyclers) == 0) {
        break()
      }
    }
    #print(winner_state)
    #}
    print(winner_state)
    ssstartPos <- used_startup_data[,. (CYCLER_ID, starting_lane, starting_row)]
    join_sinner_pos <- ssstartPos[winner_state, on = "CYCLER_ID"]
    full_action <- rbind(full_action, action_data)
    total_winner <- rbind(total_winner, join_sinner_pos)
    total_winner[, .N, by = .(POSITION, CYCLER_ID)][order(CYCLER_ID)]
    print(total_winner[, mean(POSITION), by = .(starting_lane, starting_row)][order(V1)])

    #print(full_action[, .(m)])
    print(total_winner[, mean(POSITION), by = CYCLER_ID][order(V1)])
    #full_action[, .(SLIP = mean(SLIP), BLOCK = mean(BLOCK), EXHAUST = mean(EXHAUST), ASCEND = mean(ASCEND)), by = CYCLER_ID][order(CYCLER_ID)]
    print(full_action[, .(
                          SLIP = mean(SLIP_BONUS, na.rm = TRUE), BLOCK = mean(BLOCK_DIFF, na.rm = TRUE), EXHAUST = mean(EXHAUST, na.rm = TRUE),
                          ASCEND = mean(ASCEND_GAIN, na.rm = TRUE)), by = CYCLER_ID][order(CYCLER_ID)])
  }
  #print(total_winner[, .N, by = .(CYCLER_ID, POSITION)])[order(CYCLER_ID, POSITION)]

