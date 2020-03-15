  # required_data(c("STG_CYCLER", "STG_TRACK"))
kaadu
#source("global.R")

con <- connDB(con, "flaimme")
   required_data(c("ADM_CYCLER_DECK", "ADM_OPTIMAL_MOVES", "STG_TRACK", "SRC_TRACK", "SRC_TRACK_PIECE", "STG_TRACK_PIECE", "SRC_AI_CONF", "STG_AI_CONF", "ADM_AI_CONF"), force_update =TRUE)
  total_winner <- NULL


  full_action <- NULL
  game_action <- NULL
  STG_CYCLER
  required_data("STG_TEAM")
  cycler_ids <- c(8, 4, 3, 7, 6, 2, 5, 1)

  smart_team <- 3

  smart_cycler_ids <- STG_CYCLER[TEAM_ID == smart_team, CYCLER_ID]
  game_id <- 1
  for (game_id in 1:200000) {
    track <- 26#as.integer(runif(1, 12, 17))

    winner_state <- data.table(TURN_ID = numeric(), CYCLER_ID = numeric(), POSITION = numeric(), GAME_ID = numeric(),
                               row_over_finish = numeric(), finish_square = numeric())



    game_status <- start_game(cycler_ids ,track, STG_TRACK_PIECE, STG_TRACK)
   # game_status <- randomize_start_grid(game_status)

    #initial decks
    deck_status <- create_decks(cycler_ids, ADM_CYCLER_DECK)


   #prepare log data
    move_data <- data.table(CYCLER_ID = cycler_ids, MOVEMENT = 0, CARD_ID = 0, OPTIONS = list(), key_col = "1")
    played_cards_data <- merge(x = data.table(TURN_ID = 1:25, key_col = "1"), y = move_data, by = "key_col", all = TRUE, allow.cartesian = TRUE)
    played_cards_data[, key_col := NULL]
    played_cards_data[, PHASE := 0]


    finish <- game_status[FINISH == 1, max(GAME_SLOT_ID)]
    turn_id <- 0
    pre_aggr_game_status <- precalc_track(game_status )

    con <- connDB(con, "flaimme")
    required_data("ADM_OPTIMAL_MOVES", force_update = TRUE)

    if (!exists("ctM_data")) {
      ctM_data <- NULL
    }
    turn_id <- 1


    ##################################
    #adding manual exhaust
    deck_status <- add_exhaustion(8, deck_status, "Recycle")
    repeat{
      move_range_data_actual_moves <- NULL
      orig_posits <- game_status[CYCLER_ID > 0, .(GAME_SLOT_ID, PIECE_ATTRIBUTE), by = CYCLER_ID]

      #everybody draws
      for(loop in smart_cycler_ids) {
        deck_status <- draw_cards(loop, deck_status)
       # print(list(deck_status[CYCLER_ID == loop & Zone == "Hand", CARD_ID]))

        played_cards_data[CYCLER_ID == loop & TURN_ID == turn_id,
                          OPTIONS := list(list(deck_status[CYCLER_ID == loop & Zone == "Hand", CARD_ID]))]
      }


      #CTM handling.
      # load from DB
      # if playing with humans, dont update DB
      # update only new opt results in memory that have been generated during the game

      ctM_res <- cyclers_turns_MOVEMEMENT_combs(con, ADM_OPTIMAL_MOVES, game_status, deck_status, pre_aggr_game_status)
      ctM_data <- ctM_res$ctM_data




      # if(is.null(new_EV)) {
      #   new_EV <- ctM_data[, .(Best = min(turns_to_finish), EV = sum(turns_to_finish * N) / sum(N)), by = CYCLER_ID][order(EV)]
      # }
      # prev_EV <- new_EV[, .(Best_old = Best, EV_old = EV, CYCLER_ID)]
      # new_EV <- ctM_data[, .(Best = min(turns_to_finish), EV = sum(turns_to_finish * N) / sum(N)), by = CYCLER_ID][order(EV)]
      # joinaa <- prev_EV[new_EV, on = "CYCLER_ID"]
      #
      #
      # Exha_count <- deck_status[Zone != "Removed" & MOVEMENT == 2, .(Exhaust = .N), by = CYCLER_ID]
      # Mean_move <- deck_status[Zone != "Removed" & MOVEMENT > 2 , .(SUM_POWER_MOVE = sum(MOVEMENT), MAX_MOVE = max(MOVEMENT)), by = CYCLER_ID]
      # join_ex_move <- Exha_count[Mean_move, on = "CYCLER_ID"]
      # join_tot_stat <- join_ex_move[joinaa, on = "CYCLER_ID"]
      # action_data_aggr <- action_data[, .(EXHAUST = sum(EXHAUST), BLOCK_DIFF = sum(BLOCK_DIFF),
      #                                                   ASCEND_GAIN = sum(ASCEND_GAIN),
      #                                                   SLIP_BONUS = sum(SLIP_BONUS)), by = CYCLER_ID]
      # join_stat_and_aggr <- join_tot_stat[action_data_aggr, on = "CYCLER_ID"]
      # track_left_vs_deck <- orig_posits[join_stat_and_aggr, on = "CYCLER_ID"]
      # track_left_vs_deck[, MISSING_MOVES := finish - GAME_SLOT_ID - SUM_POWER_MOVE]
      #
      # sscols_status <- track_left_vs_deck[, .(CYCLER_ID, Best, EV, MISSING_MOVES, BLOCK_DIFF, ASCEND_GAIN, SLIP_BONUS, EXHAUST, EXH_LEFT = Exhaust, MAX_MOVE, SUM_POWER_MOVE)][order(Best)]
      # #print(join_tot_stat)

      phase_loop <- 1
      repeat{

        CYCLER_ORDER <- game_status[CYCLER_ID > 0][order(-SQUARE_ID)][, .(CYCLER_ID, move_order = seq_len(.N))]
        #join team
        join_team_to_order <- STG_CYCLER[CYCLER_ORDER, on = "CYCLER_ID"]
        leading_cyclers <- join_team_to_order[join_team_to_order[, .I[which.min(move_order)], by= TEAM_ID]$V1][, CYCLER_ID]

        if (phase_loop == 1) {
        phase_ai_cyclers <- setdiff(leading_cyclers, smart_cycler_ids)
        } else {
          phase_ai_cyclers <- setdiff(played_cards_data[TURN_ID == turn_id & PHASE == 0, CYCLER_ID], smart_cycler_ids)
        }

       # pelatut_kortit <- deck_status[sample(nrow(deck_status))][Zone == "Hand", .SD[1:1], .(CYCLER_ID)][, .(CYCLER_ID, CARD_ID, MOVEMENT)][CYCLER_ID %in% cyclers]

          required_data("ADM_CYCLER_INFO")

          print(played_cards_data[TURN_ID == turn_id])

          smart_cyclers_left <- intersect(cycler_ids, smart_cycler_ids)
            if (phase_loop == 1) {
              #calculate move range before phase 1 actions
              range_joined_team <- suppressWarnings(calc_move_range(game_status, deck_status, ctM_data, STG_CYCLER))

              #until I know better, move leading cycler first
              moving_cycler <- intersect(smart_cycler_ids, leading_cyclers)
              second_cycler <- setdiff(smart_cycler_ids, moving_cycler)


              # simult_list_res <-  two_phase_simulation_score(game_status, deck_status, team_id, STG_CYCLER, turn_id,
              #                                                ctM_data, pre_aggr_game_status, range_joined_team,
              #                                                card_options = NULL, cycler_id = NULL, phase_one_actions = NULL,
              #                                                simul_rounds = 5,
              #                                                simulate_until_stopped = FALSE,
              #                                                ADM_AI_CONF = ADM_AI_CONF)
              #ctM_data <- simult_list_res$updated_ctm
             # move_first_cycler <- which_cycler_to_move_first(simult_list_res$scores, STG_CYCLER, team_id)

             # simulation_data_from_which_cycler <- move_first_cycler_result$simulation
              #look at my cards


              card_options_in_hand <- smart_cards_options(deck_status[CYCLER_ID == moving_cycler & Zone == "Hand", unique(MOVEMENT)], pre_aggr_game_status, moving_cycler)
              if (length(card_options_in_hand) == 1) {
                move_amount <- card_options_in_hand
              } else {


             # print(zoom(game_status))
                #move_amount <- max(card_options_in_hand)
                move_amount <- select_smart_card_phase_1(game_status, deck_status, smart_cycler_ids, pre_aggr_game_status, ctM_res,
                                                         range_joined_team, moving_cycler, second_cycler, smart_team, card_options_in_hand)



              }



            print(paste0("Moved cycler ", moving_cycler, ". Options: ", paste0(card_options_in_hand, collapse = " "),
                         " Played card: ", move_amount))
            } else if (phase_loop == 2 & length(smart_cyclers_left) == 2) {

              #who am i
              moving_cycler <- second_cycler
              not_moving <- setdiff(smart_cycler_ids, moving_cycler)

                card_options_in_hand_p2 <- smart_cards_options(deck_status[CYCLER_ID == moving_cycler & Zone == "Hand", unique(MOVEMENT)], pre_aggr_game_status, second_cycler)
                if (length(card_options_in_hand_p2) == 1) {
                  move_amount_p2 <- card_options_in_hand_p2
                } else {


            #  phase_one_actions <-  played_cards_data[TURN_ID == turn_id & PHASE == 1,. (CYCLER_ID, MOVEMENT, phase = PHASE)]

              move_amount <- select_smart_card_phase_2(game_status, deck_status, smart_cycler_ids,
                                                        pre_aggr_game_status, ctM_res, new_and_old_range, moving_cycler, not_moving, smart_team, phase_two_cyclers,
                                                        card_options_in_hand_p2)

              #print(aggr2)
            #  print(second_cycler)
            #  print(dcast.data.table(aggr2, formula =  Setting ~ MOVEMENT, value.var = "Result"))
                }



            print(paste0("Moved 2nd cycler ", moving_cycler, ". Options: ", paste0(card_options_in_hand_p2, collapse = " "),
                         " Played card: ", move_amount))

            }




          #############################
          for (loopperi in 1:3) {


            color <-  dlgInput("Color?")$res
            if (!color %in% c("Red", "Green", "Blue", "Purple")) {
              color <-  dlgInput("WRITE THE COLOR?")$res
            }

            R_or_S <- dlgInput("R or S?")$res

            if (!R_or_S %in% c("R", "S")) {
              R_or_S <- dlgInput("TYPE R or S?")$res
            }

            CARD_ID_input <-  as.numeric(dlgInput("Card ID")$res)
            if (toupper(R_or_S) == "S") {
              CYCLER_TYPE_NAME_input <- "Sprinteur"
            } else {
              CYCLER_TYPE_NAME_input <- "Rouler"
            }

            find_cycler <- ADM_CYCLER_INFO[CYCLER_TYPE_NAME == CYCLER_TYPE_NAME_input & TEAM_COLOR == color, CYCLER_ID]
            find_movement <- STG_CARD[CARD_ID == CARD_ID_input, MOVEMENT]
            played_cards_data[TURN_ID == turn_id & CYCLER_ID == find_cycler,  CARD_ID := CARD_ID_input]
            played_cards_data[TURN_ID == turn_id & CYCLER_ID == find_cycler, MOVEMENT := find_movement]
            played_cards_data[TURN_ID == turn_id & CYCLER_ID == find_cycler, PHASE := phase_loop]
          }


          card_id <- deck_status[CYCLER_ID == moving_cycler & MOVEMENT == move_amount, min(CARD_ID)]
          played_cards_data[CYCLER_ID  == moving_cycler & TURN_ID == turn_id, MOVEMENT := move_amount]
          played_cards_data[CYCLER_ID  == moving_cycler & TURN_ID == turn_id, PHASE := phase_loop]
          played_cards_data[TURN_ID == turn_id & CYCLER_ID == moving_cycler, CARD_ID := card_id]

          phase_cyclers <- played_cards_data[TURN_ID == turn_id & PHASE == phase_loop, CYCLER_ID]
          phase_cyclers_in_move_order <- create_move_order_vec(game_status, phase_cyclers)
            if(length(phase_cyclers_in_move_order) !=  4) {
              browser()
            }
          #####################################
          # for(loop in phase_ai_cyclers) {
          #   move_selected  <-  card_selector_by_stat(game_status, deck_status[CYCLER_ID == loop & Zone == "Hand"], loop, "SMART_MAX",  aim_downhill = TRUE)[, MOVEMENT]
          #   card_id <- deck_status[CYCLER_ID == loop & MOVEMENT == move_selected, min(CARD_ID)]
          #   played_cards_data[TURN_ID == turn_id & CYCLER_ID == loop,  CARD_ID := card_id]
          #   played_cards_data[TURN_ID == turn_id & CYCLER_ID == loop, MOVEMENT := move_selected]
          #   played_cards_data[TURN_ID == turn_id & CYCLER_ID == loop, PHASE := phase_loop]
          # }
          #####################################


          #only move cyclers in game
          in_game_cyclers <- intersect(cycler_ids, phase_cyclers_in_move_order)

      for(loop_move in in_game_cyclers) {

        row_data <- played_cards_data[TURN_ID == turn_id & loop_move == CYCLER_ID & PHASE == phase_loop]


        if (loop_move %in% smart_cycler_ids) {
        deck_status <- play_card(cycler_id = loop_move,
                                 card_id = row_data[, CARD_ID],
                                 current_decks = deck_status, 1, 1, FALSE,
                                 force = FALSE)
        } else {
          deck_status <- play_card(cycler_id = loop_move,
                                   card_id = row_data[, CARD_ID],
                                   current_decks = deck_status, 1, 1, FALSE,
                                   force = TRUE)
        }
        game_status <- move_cycler(game_status, loop_move, movement = row_data[, MOVEMENT])


        #record moves
        row_res <- range_joined_team[CYCLER_ID == row_data[, CYCLER_ID] & MOVEMENT ==  row_data[, MOVEMENT]]
        move_range_data_actual_moves <- rbind(row_res, move_range_data_actual_moves)

        print(zoom(game_status))
      }



          #calc here phase 2 range already
          phase_two_cyclers<-  played_cards_data[TURN_ID == turn_id & PHASE == 0,. (CYCLER_ID, MOVEMENT, phase = PHASE)][, CYCLER_ID]
          p2_score_outout <- phase2_slot_score(game_status, phase_two_cyclers, deck_status, range_joined_team, pre_aggr_game_status)
          range2 <- suppressWarnings(calc_move_range_phase_2(game_status, deck_status, ctM_data, p2_score_outout,
                                                             STG_CYCLER))

      range2_filter <- range2[prio_group < 90]
      #append already_moved

      new_and_old_range <- rbind(range2_filter, move_range_data_actual_moves[, .(CYCLER_ID, TEAM_ID, prio_group, MOVEMENT, odds = 1, TURNS_TO_FINISH,
                                                              actual_movement, new_slot_after_moving = new_slot, DECK_LEFT,
                                                              TRACK_LEFT, curr_pos = new_slot, splitted_odds = 1)])



          new_posits_phase <- game_status[CYCLER_ID > 0, .(GAME_SLOT_ID_NEW = GAME_SLOT_ID), by = CYCLER_ID]
          diff_phase <- orig_posits[new_posits_phase, on = "CYCLER_ID"][, ACTUAL_MOVEMENT := GAME_SLOT_ID_NEW - GAME_SLOT_ID]
          phase_2_cyclers <- diff_phase[ACTUAL_MOVEMENT == 0, CYCLER_ID]
          phase_loop <- phase_loop + 1



          if (phase_loop == 3) {
            break()
          }
      } #end of phase
      new_posits <- game_status[CYCLER_ID > 0, .(GAME_SLOT_ID_NEW = GAME_SLOT_ID), by = CYCLER_ID]
      diff <- orig_posits[new_posits, on = "CYCLER_ID"][, ACTUAL_MOVEMENT := GAME_SLOT_ID_NEW - GAME_SLOT_ID]


      game_status <- apply_slipstream(game_status)

     # print(paste0("TURN DONE ", turn_id))

  #  print(deck_status[CYCLER_ID == 3])
    #  slip_status <- game_status[CYCLER_ID > 0, .(GAME_SLOT_SLIP = GAME_SLOT_ID), by = CYCLER_ID]
    #  diff_slip <- diff[slip_status, on = "CYCLER_ID"][, ':=' (SLIP_BONUS = GAME_SLOT_SLIP - GAME_SLOT_ID_NEW,
    #                                                           TOTAL_MOVEMENT = GAME_SLOT_SLIP - GAME_SLOT_ID)]
    #  movementit <- played_cards_data[TURN_ID == turn_id,. (CYCLER_ID, MOVEMENT, PHASE)]

    #  join_exh <- calc_exhaust(game_status)[diff_slip, on = "CYCLER_ID"]
    #  join_movement <- join_exh[movementit, on = "CYCLER_ID"]
    #  action_row <- join_movement[, .(CYCLER_ID, EXHAUST, GAME_SLOT_ID = GAME_SLOT_SLIP,
    #                                  BLOCK_DIFF = pmin(ACTUAL_MOVEMENT - MOVEMENT, 0),
    #                                  ASCEND_GAIN = pmax(ACTUAL_MOVEMENT - MOVEMENT, 0),
    #                                  SLIP_BONUS,
    #                                  TOTAL_MOVEMENT, TURN_ID = turn_id)]


      deck_status <- apply_exhaustion(deck_status, game_status)


      #deck stats
     # deck_stats <- dcast.data.table(deck_status[Zone != "Removed", .(clist = list(CARD_ID)), by = .(CYCLER_ID, Zone)],
       #                             formula = CYCLER_ID ~ Zone, value.var = "clist")

      winner_state <- check_winner(game_status, winner_state, turn_id, game_id)

      cycler_ids <- setdiff(cycler_ids, winner_state[, CYCLER_ID])
      game_status <- clear_finishers(game_status,winner_state[, CYCLER_ID])
      #game_status[CYCLER_ID > 0][order(-GAME_SLOT_ID)]
      #deck_status[, .N, by = .(CYCLER_ID, Zone)][order(Zone, CYCLER_ID)]
     # deck_left <- deck_status[Zone != "Removed", .(Mean_Mov = mean(MOVEMENT)), by = CYCLER_ID]
     # deck_power <- deck_status[Zone != "Removed" & MOVEMENT > 2, .(Mean_power_mov = mean(MOVEMENT)), by = CYCLER_ID]
      #join_left_and_power <- deck_power[deck_left, on = "CYCLER_ID"]
    #  action_data <- rbind(action_data, action_row, fill = TRUE)

      if (length(cycler_ids) == 0) {
        break()
      }
      turn_id <- turn_id + 1
      print(zoom(game_status))
      print(deck_status[CYCLER_ID %in% smart_cycler_ids, .N, by = .(Zone, CYCLER_ID)])

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
  S#print(total_winner[, .N, by = .(CYCLER_ID, POSITION)])[order(CYCLER_ID, POSITION)]

