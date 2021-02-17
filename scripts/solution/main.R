  # required_data(c("STG_CYCLER", "STG_TRACK"))
#AWS docker IMAGE PITÄÄ REBUILDAA, ETTÄ TOIMII PILVESSÄ.

#source("global.R")
#kaadu
con <- connDB(con, "flaimme")
   required_data(c("STG_TEAM","ADM_CYCLER_INFO", "ADM_CYCLER_DECK", "ADM_OPTIMAL_MOVES", "STG_TRACK", "SRC_TRACK", "SRC_TRACK_PIECE", "STG_TRACK_PIECE", "SRC_AI_CONF", "STG_AI_CONF", "ADM_AI_CONF"), force_update =TRUE)
  total_winner <- NULL
game_status_data <- list()
  full_action <- NULL
  game_action <- NULL

  #playing_teams <- c(1, 2, 3, 4)
  playing_teams <- c(1, 2, 3, 4)

  smart_team <- 3
  ai_teams <- setdiff(playing_teams, smart_team)
  smart_cycler_ids <- STG_CYCLER[TEAM_ID == smart_team, CYCLER_ID]

  game_id <- 1

  for (game_id in 1:200000) {
   # cycler_ids <- c(7, 4, 5, 6, 8, 1, 3, 2)
   # cycler_ids <- c(5, 6, 1, 2)
    turn_id <- 0
    repeat {
    if (turn_id == 0) {
    turn_game_status <- NULL
    deck_status_loop <- NULL
    deck_status_loop_before <- NULL
    track <- 14#as.integer(runif(1, 12, 17))
    #ijk map explanation: i = starting point, k = movement, j = ending slot
    ijk <- ijk_map(track, STG_TRACK_PIECE, STG_TRACK)
    winner_state <- data.table(TURN_ID = numeric(), CYCLER_ID = numeric(), POSITION = numeric(), GAME_ID = numeric(),
                               row_over_finish = numeric(), finish_square = numeric())


    cycler_ids <- sample(c(1,2,3,4,5,6,7,8))
    total_cyclers <- length(cycler_ids)
    game_status <- start_game(cycler_ids ,track, STG_TRACK_PIECE, STG_TRACK)
   # game_status <- randomize_start_grid(game_status)

    #initial decks
    deck_status <- create_decks(cycler_ids, ADM_CYCLER_DECK)




   #prepare log data
    move_data <- data.table(CYCLER_ID = cycler_ids, MOVEMENT = 0, CARD_ID = 0, OPTIONS = list(), key_col = "1")
    played_cards_data <- merge(x = data.table(TURN_ID = 1:25, key_col = "1"), y = move_data, by = "key_col", all = TRUE, allow.cartesian = TRUE)
    played_cards_data[, key_col := NULL]



    finish <- game_status[FINISH == 1, max(GAME_SLOT_ID)]
    turn_id <- 0
    pre_aggr_game_status <- precalc_track(game_status )

    con <- connDB(con, "flaimme")
    required_data("ADM_OPTIMAL_MOVES", force_update = TRUE)

    if (!exists("ctM_data")) {
      ctM_data <- NULL
    }
    turn_id <- 1

    }

      in_game_cyclers <- game_status[order(-SQUARE_ID)][CYCLER_ID > 0, CYCLER_ID]

      ai_cyclers <- setdiff(cycler_ids, smart_cycler_ids)
      in_game_smart_cyclers <- setdiff(in_game_cyclers, ai_cyclers)

      #for each player, choose cycler, draw cards
      #EVERYBODY DRAWS
      for (loop_cycler in cycler_ids) {
        deck_status <- draw_cards(loop_cycler, deck_status, 4, con = con,  turn_id = turn_id, game_id = game_id)
        played_cards_data[CYCLER_ID == loop_cycler & TURN_ID == turn_id,
                          OPTIONS := list(list(deck_status[CYCLER_ID == loop_cycler & Zone == "Hand", CARD_ID]))]
      }



      #CTM handling.
      # load from DB
      # if playing with humans, dont update DB
      # update only new opt results in memory that have been generated during the game
      print(zoom(game_status))
      print(deck_status[CYCLER_ID %in% smart_cycler_ids  & Zone == "Hand"][order(CYCLER_ID, CARD_ID)])

      #are smart cyclers still playing?
      if (length(in_game_smart_cyclers) > 0) {
        # smart max if someone has finished
        if (total_cyclers != length(in_game_cyclers)) {
        # if yes, then I don't care about the moves
          for(loop in in_game_smart_cyclers) {
            card_id  <-  card_selector_by_stat(game_status, deck_status[CYCLER_ID == loop & Zone == "Hand"], loop, "SMART_MAX",  aim_downhill = TRUE)[, CARD_ID]
            played_cards_data[TURN_ID == turn_id & CYCLER_ID == loop,  CARD_ID := card_id]
          }

        } else {
          #still competing. Do we have options
          # dt_cyclers <- data.table(CYCLER_ID = cycler_ids)
          # dt_cyclers[, options := list(list(smart_cards_options(deck_status, pre_aggr_game_status, CYCLER_ID, game_status))), by = CYCLER_ID]
          # expand_lis <- dt_cyclers[ , .(MOVEMENT = unlist( options ), SMART_MOVE = TRUE) , by = CYCLER_ID ]
          # join_to_ds <- expand_lis[deck_status, on =  .(CYCLER_ID, MOVEMENT)]
          # join_to_ds[Zone == "Hand" & is.na(SMART_MOVE), Zone := "Recycle"]
          # deck_status <- join_to_ds

          ctM_res <- cyclers_turns_MOVEMEMENT_combs(con, ADM_OPTIMAL_MOVES, game_status, deck_status, pre_aggr_game_status)
          ctM_data <- ctM_res$ctM_data


          #calculate mixed strategies

          MIXED_STRATEGY <- calculate_mixed_strategy(game_status, deck_status, ijk, ADM_AI_CONF, ADM_OPTIMAL_MOVES, ctM_data, STG_TEAM)
          setkey(MIXED_STRATEGY, PROB_PRODUCT)
          ress <- MIXED_STRATEGY[, tail(.SD, 3), by = TEAM_ID]
          ress[, prio := seq_len(.N), by = TEAM_ID]
          ress[order(TEAM_ID, -prio )]
          #CHOOSE WHICH DECISION MAKE FIRST. The one with bettter winning chance goes last

          #moving_cycler <- choose_first_AI_cycler(smart_team, game_status, "leading", STG_CYCLER)
          #rouler first
          #  moving_cycler <- ADM_CYCLER_INFO[CYCLER_ID %in% smart_cycler_ids & CYCLER_ID %in% in_game_cyclers, CYCLER_ID]
          moving_cycler_dt <- ADM_CYCLER_INFO[TEAM_ID == smart_team & CYCLER_ID %in% in_game_cyclers, .(CYCLER_ID = min(CYCLER_ID))]
          #simulate and score

          if (nrow(moving_cycler_dt) > 0) {
            moving_cycler <- moving_cycler_dt[, CYCLER_ID]

            # card_id_selected  <-  card_selector_by_stat(game_status, deck_status[CYCLER_ID == moving_cycler & Zone == "Hand"], moving_cycler, "SMART_MAX",  aim_downhill = TRUE)[, CARD_ID]
            options <- deck_status[CYCLER_ID == moving_cycler & Zone == "Hand", .N, by = .(MOVEMENT)][, MOVEMENT]
            cards_in_hand <- MIXED_STRATEGY[C1 == moving_cycler & M1 %in% options,
                                            .N, by = .(M1, MOVES, PROB_PRODUCT, EV,
                                                       MOVE_DIFF_SCORE,
                                                       EXHAUST_SCORE,
                                                       TTF_SCORE,
                                                       TURNS_TO_FINISH,
                                                    #   CYC_DIST_SCORE,
                                                       MOVE_ORDER_SCORE,
                                                       OVER_FINISH_SCORE)][, N := NULL]
            print(cards_in_hand)
            EV <- cards_in_hand[, .(EV = sum(EV *  PROB_PRODUCT) / sum(PROB_PRODUCT)), by = M1]

            first_movement <- EV[, M1[which.max(EV)]]
            #convert exhaust if possible
            first_move <- deck_status[CYCLER_ID == moving_cycler & Zone == "Hand" & MOVEMENT == first_movement, min(CARD_ID)]
            print(paste0("rouler played: ", first_move))
            played_cards_data[TURN_ID == turn_id & CYCLER_ID == moving_cycler,  CARD_ID := first_move]
          }
          #CHOOSE FIRST CARD
          #    move_amount <- select_smart_card_phase(game_status, deck_status, smart_cycler_ids, pre_aggr_game_status, ctM_res,
          #                                       range_joined_team, moving_cycler, second_cycler, smart_team, card_options_in_hand)


          #CHOOSE SECOND CARD


          second_cycler_in_team <- ADM_CYCLER_INFO[TEAM_ID == smart_team & CYCLER_ID %in% in_game_cyclers & !CYCLER_ID %in% moving_cycler , .(CYCLER_ID = min(CYCLER_ID))]
          #still playing?
          #  second_cycler <- intersect(cycler_ids, second_cycler_in_team)
          if (nrow(second_cycler_in_team) == 1) {
            second_cycler <- second_cycler_in_team[, CYCLER_ID]
            # card_id_2nd  <-  card_selector_by_stat(game_status, deck_status[CYCLER_ID == second_cycler & Zone == "Hand"], second_cycler, "SMART_MAX",  aim_downhill = TRUE)[, CARD_ID]
            options <- deck_status[CYCLER_ID == second_cycler & Zone == "Hand", .N, by = MOVEMENT][, MOVEMENT]
            cards_in_hand <- MIXED_STRATEGY[M1 == first_movement & C2 == second_cycler & M2 %in% options,
                                            .N, by = .(M2, MOVES, PROB_PRODUCT, EV,
                                                       MOVE_DIFF_SCORE,
                                                       EXHAUST_SCORE,
                                                       TTF_SCORE,
                                                       TURNS_TO_FINISH,
                                                   #    CYC_DIST_SCORE,
                                                       MOVE_ORDER_SCORE,
                                                       OVER_FINISH_SCORE)][, N := NULL]



            print(cards_in_hand)
            second_movement <-  cards_in_hand[, M2[which.max(EV)]]
            second_move <- deck_status[CYCLER_ID == second_cycler & Zone == "Hand" & MOVEMENT == second_movement, min(CARD_ID)]
            print(paste0("sprinteur played: ", second_move))


            played_cards_data[TURN_ID == turn_id & CYCLER_ID == second_cycler,  CARD_ID := second_move]
          }

        }

      }





  #choose ai cycler cards

          for(loop in ai_cyclers) {
            card_id  <-  card_selector_by_stat(game_status, deck_status[CYCLER_ID == loop & Zone == "Hand"], loop, "SMART_MAX",  aim_downhill = TRUE)[, CARD_ID]
            played_cards_data[TURN_ID == turn_id & CYCLER_ID == loop,  CARD_ID := card_id]
          }






      for(loop_move in in_game_cyclers) {

        row_data <- played_cards_data[TURN_ID == turn_id & loop_move == CYCLER_ID]
        move_played <- STG_CARD[CARD_ID == row_data[, CARD_ID], MOVEMENT]
          deck_status <- play_card(cycler_id = loop_move,
                                   card_id = row_data[, CARD_ID],
                                   current_decks = deck_status, game_id, turn_id, con,
                                   force = TRUE,
                                   copy = FALSE)

          game_status <- move_cycler(game_status, loop_move, move_played, slipstream = FALSE,
                                     ignore_block = FALSE,
                                     ignore_end_of_track = FALSE,
                                     return_numeric_position = FALSE)

      }

      #SLIP STREAM
      game_status <- apply_slipstream(game_status)

     #EXHAUST
      deck_status <- apply_exhaustion(deck_status, game_status)

      #RECROD WINNER
      winner_state <- check_winner(game_status, winner_state, turn_id, game_id)

      #remove finished
      game_status <- clear_finishers(game_status,winner_state[, CYCLER_ID])
      #update who is left
      cycler_ids <- game_status[CYCLER_ID >0, CYCLER_ID]

      # if (nrow(winner_state) > 0) {
      #   break()
      # }
      if (length(cycler_ids) == 0) {
        break()
      }

      turn_game_status[[turn_id]] <- copy(game_status)
      deck_status_loop[[turn_id]] <- copy(deck_status)
      turn_id <- turn_id + 1
      #print(zoom(game_status))
    }

    game_status_data[[game_id]] <- turn_game_status
    #print(winner_state)
    #}

    print(winner_state)
  #  ssstartPos <- used_startup_data[,. (CYCLER_ID, starting_lane, starting_row)]
  #  join_sinner_pos <- ssstartPos[winner_state, on = "CYCLER_ID"]
  #  full_action <- rbind(full_action, action_data)
    total_winner <- rbind(total_winner, winner_state)
  #  total_winner[, .N, by = .(POSITION, CYCLER_ID)][order(CYCLER_ID)]
  # print(total_winner[, mean(POSITION), by = .(starting_lane, starting_row)][order(V1)])

    #print(full_action[, .(m)])
    print(total_winner[, mean(POSITION), by = CYCLER_ID][order(V1)])
    #full_action[, .(SLIP = mean(SLIP), BLOCK = mean(BLOCK), EXHAUST = mean(EXHAUST), ASCEND = mean(ASCEND)), by = CYCLER_ID][order(CYCLER_ID)]
  #  print(full_action[, .(
   #                       SLIP = mean(SLIP_BONUS, na.rm = TRUE), BLOCK = mean(BLOCK_DIFF, na.rm = TRUE), EXHAUST = mean(EXHAUST, na.rm = TRUE),
  #                        ASCEND = mean(ASCEND_GAIN, na.rm = TRUE)), by = CYCLER_ID][order(CYCLER_ID)])

    turn_id <- 0
  }
#print(total_winner[, .N, by = .(CYCLER_ID, POSITION)])[order(CYCLER_ID, POSITION)]

