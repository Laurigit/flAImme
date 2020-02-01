  # required_data(c("STG_CYCLER", "STG_TRACK"))
  # input_STARTUP_DATA <- data.table(CYCLER_ID = c(1,2,3,4,5,6,7,8),
  #                                  PLAYER_ID = c(1,1,2,2,3,3,4,4),
  #                                  exhaust = c(0, 0, 0, 0, 0, 0,0,0),
  #                                  starting_row =   c(1, 1, 1, 1, 1, 1,1,1),
  #                                   starting_lane = c(1,2, 3, 4, 5, 6,7,8))
  #  track <- 3

 # cycler_player_dt <- data.table(CYCLER_ID = c(1,2,3,4,5,6,7,8), PLAYER_ID = c(1,1,2,2,3,3,4,4))
# lane_data <-    data.table(  exhaust = c(0, 0, 0, 0, 0, 0,0,0),
#                                  starting_row =   c(1, 1, 2, 2, 3, 3, 4, 4),
#                                   starting_lane = c(1,2, 1, 2, 1, 2, 1, 2))
   required_data(c("STG_TRACK", "SRC_TRACK", "SRC_TRACK_PIECE", "STG_TRACK_PIECE", "SRC_AI_CONF", "STG_AI_CONF", "ADM_AI_CONF"), force_update =TRUE)
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
    orig_posits <- game_status[CYCLER_ID > 0, .(GAME_SLOT_ID, PIECE_ATTRIBUTE), by = CYCLER_ID]

    #initial decks
    deck_status <- create_decks(input_STARTUP_DATA[, CYCLER_ID], ADM_CYCLER_DECK)


    cyclers <- input_STARTUP_DATA[, CYCLER_ID]
    action_data <- NULL
    #draw phase
    turn_amount <- 25
    move_data <- data.table(CYCLER_ID = cyclers, MOVEMENT = NA, CARD_ID = NA, key_col = "1")
    played_cards_data <- merge(x = data.table(TURN_ID = 1:turn_amount, key_col = "1"), y = move_data, by = "key_col", all = TRUE, allow.cartesian = TRUE)
    played_cards_data[, key_col := NULL]
    for(turn_id in 1:turn_amount) {
      #print(turn_id)

      #everybody draws
      for(loop in cyclers) {
        deck_status <- draw_cards(loop, deck_status)
      }
      smart_cyclers <- c(5, 6)
      ai_cyclers <- setdiff(cyclers, smart_cyclers)
      #everybody draws
     # pelatut_kortit <- deck_status[sample(nrow(deck_status))][Zone == "Hand", .SD[1:1], .(CYCLER_ID)][, .(CYCLER_ID, CARD_ID, MOVEMENT)][CYCLER_ID %in% cyclers]
      for(loop in ai_cyclers) {
        played_cards_data[TURN_ID == turn_id & CYCLER_ID == loop,  CARD_ID := card_selector_by_stat(game_status, deck_status[CYCLER_ID == loop & Zone == "Hand"], loop, "SMART_MAX",  aim_downhill = TRUE)[, MOVEMENT]]
        played_cards_data[TURN_ID == turn_id & CYCLER_ID == loop, MOVEMENT := CARD_ID]
      }

      #action_data <- data.table(CYCLER_ID = c(1,2,3,4,5,6), MOVEMENT = c(2,3,2,5,4,3), move_order = c(1,2,3,4,5,6), TEAM_ID = c(1,1,2,2,3,3))



          action_data3 <- pelatut_kortit[,. (CYCLER_ID, 20)]
          CYCLER_ORDER <- game_status[CYCLER_ID > 0][order(-SQUARE_ID)][, .(CYCLER_ID, move_order = seq_len(.N))]
          action_data2 <- CYCLER_ORDER[action_data3, on = "CYCLER_ID"]
          ss_team <- STG_CYCLER[, .(CYCLER_ID, TEAM_ID)]
          action_data1 <- ss_team[action_data2, on = "CYCLER_ID"]
          #cycler 1 hand
          for(cycler_loop in smart_cyclers) {
            if (nrow(winner_state[CYCLER_ID == cycler_loop]) == 0) {
              colnames <- c("CYCLER_ID", "MOVEMENT", "CARD_ID")
              played_cards_data[CYCLER_ID == cycler_loop & TURN_ID == turn_id, (colnames) := choose_AI_card(cycler_loop, deck_status, action_data)]
            }
            smart_res_total <- rbind(smart_result_lo)
          }
          smart_res_total <-




          if (nrow(winner_state[CYCLER_ID == 5]) == 0) {
          my_hand <- deck_status[CYCLER_ID == 5 & Zone == "Hand"]
          best_move <- NULL
          tot_analysis <- data.table(Setting = "")
          # orig_speed <- turns_to_finish(game_status, deck_status)
          for (best_loop in my_hand[, .N, by = MOVEMENT][, MOVEMENT]) {
            action_data1[CYCLER_ID == 5, MOVEMENT := best_loop]
            pos_score <- suppressWarnings(score_position(game_status, deck_status, action_data1, ADM_AI_CONF, orig_speed))[!is.na(Score)]
            my_score <- pos_score[CYCLER_ID == 5, sum(Result)]
            #print(paste0("MY ", my_score))
            my_analysis <- pos_score[CYCLER_ID == 5, sum(Result), by = Setting]
            setnames(my_analysis, "V1", paste0("M",best_loop))
            tot_analysis <- tot_analysis[my_analysis, on = "Setting"]
            # print( pos_score[CYCLER_ID == 5])
            #kovin_vrihu <- AI(cycler_id = 5, game_status, deck_status)
            enemy_score <- pos_score[CYCLER_ID != 5, sum(Result), by = CYCLER_ID][, mean(V1)]
            #print(paste0("ENEM ", enemy_score))
            # print( pos_score[CYCLER_ID != 5, sum(Result), by = Setting])
            # print(tot_analysis)
            row_data <- data.table(tot_score = my_score - 0, MOVEMENT = best_loop)
            best_move <- rbind(row_data, best_move)
            #print(pos_score[CYCLER_ID == 5])
          }

          #  print(tot_analysis)
          selected_move <- best_move[ , .SD[which.max(tot_score)]][, MOVEMENT]
         # print(zoom(game_status))
          print(paste0(selected_move, " - ", paste0( my_hand[, .N, by = MOVEMENT][, MOVEMENT], collapse = ",")))


          #browser()
          pelatut_kortit[CYCLER_ID == 5, ':=' (MOVEMENT = selected_move, CARD_ID = selected_move) ]

          }

      #
      #print("Start moving")
      orig_posits <- game_status[CYCLER_ID > 0, .(GAME_SLOT_ID, PIECE_ATTRIBUTE), by = CYCLER_ID]
      for(loop_move in cyclers) {
        row_data <- pelatut_kortit[loop_move == CYCLER_ID]

        deck_status <- play_card(cycler_id = loop_move,
                                 card_id = row_data[, CARD_ID],
                                 current_decks = deck_status, 1, 1, FALSE)
        game_status <- move_cycler(game_status, loop_move, movement = row_data[, MOVEMENT])

      }
      new_posits <- game_status[CYCLER_ID > 0, .(GAME_SLOT_ID_NEW = GAME_SLOT_ID), by = CYCLER_ID]
      diff <- orig_posits[new_posits, on = "CYCLER_ID"][, ACTUAL_MOVEMENT := GAME_SLOT_ID_NEW - GAME_SLOT_ID]

      game_status <- apply_slipstream(game_status)
      slip_status <- game_status[CYCLER_ID > 0, .(GAME_SLOT_SLIP = GAME_SLOT_ID), by = CYCLER_ID]
      diff_slip <- diff[slip_status, on = "CYCLER_ID"][, ':=' (SLIP_BONUS = GAME_SLOT_SLIP - GAME_SLOT_ID_NEW,
                                                               TOTAL_MOVEMENT = GAME_SLOT_SLIP - GAME_SLOT_ID)]
      movementit <- pelatut_kortit[,. (CYCLER_ID, MOVEMENT)]

      join_exh <- calc_exhaust(game_status)[diff_slip, on = "CYCLER_ID"]
      join_movement <- join_exh[movementit, on = "CYCLER_ID"]
      action_row <- join_movement[, .(CYCLER_ID, EXHAUST, GAME_SLOT_ID = GAME_SLOT_SLIP,
                                      BLOCK_DIFF = pmin(ACTUAL_MOVEMENT - MOVEMENT, 0),
                                      ASCEND_GAIN = pmax(ACTUAL_MOVEMENT - MOVEMENT, 0),
                                      SLIP_BONUS, START_PIECE = PIECE_ATTRIBUTE,
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
  print(total_winner[, .N, by = .(CYCLER_ID, POSITION)])[order(CYCLER_ID, POSITION)]

