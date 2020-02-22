  # required_data(c("STG_CYCLER", "STG_TRACK"))
  # input_STARTUP_DATA <- data.table(CYCLER_ID = c(1,2,3,4,5,6,7,8),
  #                                  PLAYER_ID = c(1,1,2,2,3,3,4,4),
  #                                  exhaust = c(0, 0, 0, 0, 0, 0,0,0),
  #                                  starting_row =   c(1, 1, 2, 2, 3, 3,4,4),
  #                                   starting_lane = c(1,2, 1, 2, 1, 2,1,2))
  #  track <- 3
   # input_STARTUP_DATA <- data.table(CYCLER_ID = c(1,2,3,4),
   #                                  PLAYER_ID = c(1,1,2,2),
   #                                  exhaust = c(0, 0, 0, 0),
   #                                  starting_row =   c(1, 1, 2, 2),
   #                                  starting_lane = c(1,2, 1, 2))
   # track <- 3

# cycler_player_dt <- data.table(CYCLER_ID = c(1,2,3,4,5,6,7,8), PLAYER_ID = c(1,1,2,2,3,3,4,4))
# lane_data <-    data.table(  exhaust = c(0, 0, 0, 0, 0, 0,0,0),
#                                  starting_row =   c(1, 1, 2, 2, 3, 3, 4, 4),
#                                   starting_lane = c(1,2, 1, 2, 1, 2, 1, 2))
con <- connDB(con, "flaimme")
   required_data(c("ADM_OPTIMAL_MOVES", "STG_TRACK", "SRC_TRACK", "SRC_TRACK_PIECE", "STG_TRACK_PIECE", "SRC_AI_CONF", "STG_AI_CONF", "ADM_AI_CONF"), force_update =TRUE)
  total_winner <- NULL


  full_action <- NULL
  game_action <- NULL
  for (game_id in 1:200000) {
    winner_state <- data.table(TURN_ID = numeric(), CYCLER_ID = numeric(), POSITION = numeric(), GAME_ID = numeric(),
                               row_over_finish = numeric(), finish_square = numeric())

    if (is.null(total_winner)) {
      used_startup_data <- input_STARTUP_DATA
    } else{
      stats <- total_winner[, mean(POSITION), by = CYCLER_ID][order(V1)]
      stats[, rank := seq_len(.N)]
      join_stats <- stats[cycler_player_dt, on = "CYCLER_ID"][order(-rank)]
      cbindaa <- cbind(join_stats, lane_data)
      sscols_startup <- cbindaa[, .(CYCLER_ID, PLAYER_ID, exhaust, starting_lane, starting_row)]
      used_startup_data <- sscols_startup
    }



    game_status <- start_game(used_startup_data,1, STG_TRACK_PIECE, STG_TRACK)
    #add_mountain_info
    game_status <- slots_out_of_mountains(game_status)
    game_status <- slots_out_of_mountains_to_track(game_status)
    orig_posits <- game_status[CYCLER_ID > 0, .(GAME_SLOT_ID, PIECE_ATTRIBUTE), by = CYCLER_ID]

    #initial decks
    deck_status <- create_decks(input_STARTUP_DATA[, CYCLER_ID], ADM_CYCLER_DECK)


    cyclers <- input_STARTUP_DATA[, CYCLER_ID]
    smart_cyclers <- c(3, 4)
    action_data <- NULL
    #draw phase
    turn_amount <- 25
    move_data <- data.table(CYCLER_ID = cyclers, MOVEMENT = 0, CARD_ID = 0, key_col = "1")
    played_cards_data <- merge(x = data.table(TURN_ID = 1:turn_amount, key_col = "1"), y = move_data, by = "key_col", all = TRUE, allow.cartesian = TRUE)
    played_cards_data[, key_col := NULL]
    played_cards_data[, PHASE := 0]
    phase_data <- data.table(CYCLER_ID = cyclers, phase = cyclers %% 2 + 1 )
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
      for (phase_loop in 1:2) {

        phase_cyclers <- phase_data[phase == phase_loop & CYCLER_ID %in% cyclers, CYCLER_ID]
        ai_cyclers <-  setdiff(phase_cyclers, smart_cyclers)


       # pelatut_kortit <- deck_status[sample(nrow(deck_status))][Zone == "Hand", .SD[1:1], .(CYCLER_ID)][, .(CYCLER_ID, CARD_ID, MOVEMENT)][CYCLER_ID %in% cyclers]
      for(loop in ai_cyclers) {
        played_cards_data[TURN_ID == turn_id & CYCLER_ID == loop,  CARD_ID := card_selector_by_stat(game_status, deck_status[CYCLER_ID == loop & Zone == "Hand"], loop, "SMART_MAX",  aim_downhill = TRUE)[, MOVEMENT]]
        played_cards_data[TURN_ID == turn_id & CYCLER_ID == loop, MOVEMENT := CARD_ID]
        played_cards_data[TURN_ID == turn_id & CYCLER_ID == loop, PHASE := phase_loop]
      }

      #action_data <- data.table(CYCLER_ID = c(1,2,3,4,5,6), MOVEMENT = c(2,3,2,5,4,3), move_order = c(1,2,3,4,5,6), TEAM_ID = c(1,1,2,2,3,3))



          action_data3 <- played_cards_data[TURN_ID == turn_id,. (CYCLER_ID, 0)]
          CYCLER_ORDER <- game_status[CYCLER_ID > 0][order(-SQUARE_ID)][, .(CYCLER_ID, move_order = seq_len(.N))]
          action_data2 <- CYCLER_ORDER[action_data3, on = "CYCLER_ID"]
          ss_team <- STG_CYCLER[, .(CYCLER_ID, TEAM_ID)]
          action_data1 <- ss_team[action_data2, on = "CYCLER_ID"]
          #cycler 1 hand



          smart_phase_cycler <- setdiff(smart_cyclers, phase_cyclers)
          team_id <- 2

            if (phase_loop == 1) {
              move_first_cycler_result <- which_cycler_to_move_first(game_status, deck_status, team_id, STG_CYCLER, turn_id, ctM_data, pre_aggr_game_status)

              move_first_cycler <- move_first_cycler_result$decision
              simulation_data_from_which_cycler <- move_first_cycler_result$simulation
              #look at my cards

              print(paste0("Moving cycler ", move_first_cycler))
              card_options_in_hand <- smart_cards_options(deck_status[CYCLER_ID == move_first_cycler & Zone == "Hand", unique(MOVEMENT)], pre_aggr_game_status, move_first_cycler)


              print(card_options_in_hand)
             # print(zoom(game_status))

           simul_res_p1 <-  simulate_and_scores_phase_1(game_status, deck_status, team_id, STG_CYCLER, ctM_data, card_options_in_hand, move_first_cycler, simulation_data_from_which_cycler)
            move_cyc <- move_first_cycler
            move_amount <-  simul_res_p1
            print(paste0("Played card: ", move_amount))
            played_cards_data[CYCLER_ID  == move_first_cycler & TURN_ID == turn_id, MOVEMENT := move_amount]
            played_cards_data[CYCLER_ID  == move_first_cycler & TURN_ID == turn_id, PHASE := phase_loop]
            played_cards_data[TURN_ID == turn_id & CYCLER_ID == move_first_cycler, CARD_ID := move_amount]
            } else if (phase_loop == 2) {
              #who am i
              second_cycler <- STG_CYCLER[TEAM_ID == STG_CYCLER[CYCLER_ID == move_first_cycler, TEAM_ID] & CYCLER_ID != move_first_cycler, CYCLER_ID]

                card_options_in_hand_p2 <- smart_cards_options(deck_status[CYCLER_ID == second_cycler & Zone == "Hand", unique(MOVEMENT)], pre_aggr_game_status, second_cycler)


                print(paste0("Phase 2, moving cycler ", second_cycler))

                print(card_options_in_hand_p2)
                print(zoom(game_status))

              phase_one_actions <-  played_cards_data[TURN_ID == turn_id & MOVEMENT & PHASE == 1,. (CYCLER_ID, MOVEMENT)]
            simul_rs_p2 <-  simulate_and_scores_phase_2(game_status, deck_status, second_cycler, STG_CYCLER
                                                        , ctM_data, phase_one_actions, pre_aggr_game_status, card_options_in_hand_p2)
            move_cyc <- simul_rs_p2[, CYCLER_ID]
            move_amount <-  simul_rs_p2[, MOVEMENT]
            print(paste0("Played card: ", move_amount))
            played_cards_data[CYCLER_ID  == move_cyc & TURN_ID == turn_id, MOVEMENT := move_amount]
            played_cards_data[CYCLER_ID  == move_cyc & TURN_ID == turn_id, PHASE := phase_loop]
            played_cards_data[TURN_ID == turn_id & CYCLER_ID == move_cyc, CARD_ID := move_amount]

            }


          phase_cyclers <- played_cards_data[TURN_ID == turn_id & PHASE == phase_loop, CYCLER_ID]


      for(loop_move in phase_cyclers) {
       # browser()
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

      print("TURN DONE")
      print(zoom(game_status))
    print(deck_status[CYCLER_ID == 3])
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

      action_data <- rbind(action_data, action_row, fill = TRUE)

      deck_status <- apply_exhaustion(deck_status, game_status)
      winner_state <- check_winner(game_status, winner_state, turn_id, game_id)
      cyclers <- setdiff(cyclers, winner_state[, CYCLER_ID])
      game_status <- clear_finishers(game_status,winner_state[, CYCLER_ID])
      #game_status[CYCLER_ID > 0][order(-GAME_SLOT_ID)]
      #deck_status[, .N, by = .(CYCLER_ID, Zone)][order(Zone, CYCLER_ID)]
      deck_left <- deck_status[Zone != "Removed", .(Mean_Mov = mean(MOVEMENT)), by = CYCLER_ID]
      deck_power <- deck_status[Zone != "Removed" & MOVEMENT > 2, .(Mean_power_mov = mean(MOVEMENT)), by = CYCLER_ID]
      join_left_and_power <- deck_power[deck_left, on = "CYCLER_ID"]

      action_data <- join_left_and_power[action_data, on = "CYCLER_ID"]
      aggr_action <- action_data[, .(Mean_power_mov = mean(Mean_power_mov),
                                     Mean_Mov = mean(Mean_Mov),
                                     SLIP = sum(SLIP_BONUS), BLOCK = sum(BLOCK_DIFF), EXHAUST = sum(EXHAUST), ASCEND = sum(ASCEND_GAIN)), by = CYCLER_ID][order(CYCLER_ID)][, GAME_ID := game_id]
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
    full_action <- rbind(full_action, aggr_action)
    total_winner <- rbind(total_winner, winner_state)
    total_winner[, .N, by = .(POSITION, CYCLER_ID)][order(CYCLER_ID)]
    #print(full_action[, .(m)])
    print(total_winner[, mean(POSITION), by = CYCLER_ID][order(V1)])
    #full_action[, .(SLIP = mean(SLIP), BLOCK = mean(BLOCK), EXHAUST = mean(EXHAUST), ASCEND = mean(ASCEND)), by = CYCLER_ID][order(CYCLER_ID)]
    print(full_action[, .(Mean_power_mov = mean(Mean_power_mov, na.rm = TRUE),
                          Mean_Mov = mean(Mean_Mov, na.rm = TRUE), SLIP = mean(SLIP), BLOCK = mean(BLOCK), EXHAUST = mean(EXHAUST), ASCEND = mean(ASCEND)), by = CYCLER_ID][order(CYCLER_ID)])
  }
  #print(total_winner[, .N, by = .(CYCLER_ID, POSITION)])[order(CYCLER_ID, POSITION)]

