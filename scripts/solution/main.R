  # required_data(c("STG_CYCLER", "STG_TRACK"))
#AWS docker IMAGE PITÄÄ REBUILDAA, ETTÄ TOIMII PILVESSÄ.

source("global.R")
#kaadu
con <- connDB(con, "flaimme")
   required_data(c("STG_TEAM","ADM_CYCLER_INFO", "ADM_CYCLER_DECK", "ADM_OPTIMAL_MOVES", "STG_TRACK", "SRC_TRACK", "SRC_TRACK_PIECE", "STG_TRACK_PIECE", "SRC_AI_CONF", "STG_AI_CONF", "ADM_AI_CONF"), force_update =TRUE)
  total_winner <- NULL
game_status_data <- list()
  full_action <- NULL
  game_action <- NULL


  game_id <- 1

 # bot_data <- data.table(bot_name = c("finish_rank_bot", "ttf_bot", "slots_over_bot"), TEAM_ID = c(2, 3, 4))
  bot_data <- data.table(bot_name = c("finish_rank_bot"), TEAM_ID = c(2))
  #bot_data <- data.table(bot_name = c("slots_over_bot"), TEAM_ID = c( 4))


  for (game_id in 1:200000) {
   # cycler_ids <- c(7, 4, 5, 6, 8, 1, 3, 2)
   # cycler_ids <- c(5, 6, 1, 2)
    turn_id <- 0
    repeat {
    if (turn_id == 0) {
    turn_game_status <- NULL
    deck_status_loop <- NULL
    deck_status_loop_before <- NULL
    track <- sample(c(1,2,3,6,7,19,20,36,37,39,40,41,42),1)#as.integer(runif(1, 12, 17))
    #ijk map explanation: i = starting point, k = movement, j = ending slot


    winner_state <- data.table(TURN_ID = numeric(), CYCLER_ID = numeric(), POSITION = numeric(), GAME_ID = numeric(),
                               row_over_finish = numeric(), finish_square = numeric())


    cycler_ids <- sample(c(1,2,3,4,5,6,7,8))
  #  cycler_ids <- sample(c(3,4))
  #  cycler_ids <- sample(c(3,4,5,6))
    total_cyclers <- length(cycler_ids)
    game_status <- start_game(cycler_ids ,track, STG_TRACK_PIECE, STG_TRACK)
   # game_status <- randomize_start_grid(game_status)

    #initial decks
    deck_status <- create_decks(cycler_ids, ADM_CYCLER_DECK)

    ijk <- ijk_map(track, STG_TRACK_PIECE, STG_TRACK)

    matr_ijk <- as.matrix(ijk)
    slip_map <- slipstream_map(track, STG_TRACK_PIECE, STG_TRACK)
    slip_map_matrix <- as.matrix(slip_map)

    slots_squares <- as.matrix(game_status[, .(SQUARE_ID, GAME_SLOT_ID)])
    reverse_slots_squares <- slots_squares[nrow(slots_squares):1,]
    # reverse_slip_map <- slip_map_matrix[nrow(slip_map_matrix):1,]


   #prepare log data
    move_data <- data.table(CYCLER_ID = cycler_ids, MOVEMENT = 0, CARD_ID = 0, OPTIONS = list(), key_col = "1")
    played_cards_data <- merge(x = data.table(TURN_ID = 1:25, key_col = "1"), y = move_data, by = "key_col", all = TRUE, allow.cartesian = TRUE)
    played_cards_data[, key_col := NULL]


    TTF_stats <- NULL
    finish <- game_status[FINISH == 1, max(GAME_SLOT_ID)]
    turn_id <- 0
    pre_aggr_game_status <- precalc_track(game_status)

    con <- connDB(con, "flaimme")
    required_data("ADM_OPTIMAL_MOVES", force_update = TRUE)

    if (!exists("ctM_data")) {
      ctM_data <- NULL
    }
    turn_id <- 1

    }

      in_game_cyclers <- game_status[order(-SQUARE_ID)][CYCLER_ID > 0, CYCLER_ID]



      #for each player, choose cycler, draw cards
      #EVERYBODY DRAWS
      ##take copy of deck before drawing for calculation which cycler to play first
      turn_start_deck <- copy(deck_status)
      for (loop_cycler in in_game_cyclers) {
        deck_status <- draw_cards(loop_cycler, deck_status, 4, con = NULL,  turn_id = turn_id, game_id = game_id)
        played_cards_data[CYCLER_ID == loop_cycler & TURN_ID == turn_id,
                          OPTIONS := list(list(deck_status[CYCLER_ID == loop_cycler & Zone == "Hand", CARD_ID]))]
      }



      #CTM handling.
      # load from DB
      # if playing with humans, dont update DB
      # update only new opt results in memory that have been generated during the game

      #are smart cyclers still playing?

        # smart max if someone has finished




          bot_teams <- bot_data[, TEAM_ID]
          bots <- STG_CYCLER[TEAM_ID %in% bot_teams, CYCLER_ID]
          bots_left_total <- intersect(bots, in_game_cyclers)
          #any bots left?
          if (length(bots_left_total) > 0) {
            pre_agg_no_list <- pre_aggr_game_status$aggr_to_slots
            combinations_output <- calc_combinations_data(con, game_status, turn_start_deck,
                                                          pre_agg_no_list, matr_ijk, reverse_slots_squares, slip_map_matrix, STG_CYCLER, calc_ttf)
            MIXED_STRATEGY <- calculate_mixed_strategy(combinations_output, consensus_config_id = NA, turn_start_deck)
            print(MIXED_STRATEGY$combinations[, .N, by = SLOTS_OVER_FINISH])

            ss_for_picture <- MIXED_STRATEGY$combinations[, .(FINISH_ESTIMATE_MEAN , CYCLER_ID, PROB_PRODUCT, case_id, NEW_GAME_SLOT_ID, CASE_PROB )]
           # ss_for_picture[, CASE_PROB := prod(PROB_PRODUCT^(1/2)), by = case_id]

            aggr_pic <- ss_for_picture[, .(FINISH_ESTIMATE_MEAN = sum(FINISH_ESTIMATE_MEAN * CASE_PROB) / sum(CASE_PROB),
                                           NEW_GAME_SLOT_ID = sum(NEW_GAME_SLOT_ID  * CASE_PROB) / sum(CASE_PROB)), by = CYCLER_ID]

            aggr_pic[, TURN_ID := turn_id]
            if (turn_id == 1) {
              acnhor_turn <- aggr_pic[, min(FINISH_ESTIMATE_MEAN)]
            }
            aggr_pic[, TTF_SCALED := FINISH_ESTIMATE_MEAN - acnhor_turn + TURN_ID - 1, by = .(TURN_ID)]
            aggr_pic[, CYC_TYPE := CYCLER_ID  %% 2]

            TTF_stats <- rbind(TTF_stats, aggr_pic)
            finishssi <- game_status[FINISH == 1, min(GAME_SLOT_ID)]
            startti <-game_status[START == 1, min(GAME_SLOT_ID)] + 2
            print(ggplot(data=TTF_stats, aes(x=NEW_GAME_SLOT_ID, y=TTF_SCALED, group=CYCLER_ID)) +
                    #geom_line(linetype="dashed", color="blue", size=1.2)+
                    geom_line(size=1.5, aes(linetype = "solid", color=as.factor(CYCLER_ID)))+
                    geom_point(size = 3, aes(color=as.factor(CYCLER_ID), shape=as.factor(CYC_TYPE))) +
                     scale_color_manual(values=c("red", "red", "blue", "blue", "black", "black", "green", "green")) +
                    xlim(6, finishssi) + ylim(-1.5, 3.2)) + scale_x_continuous(limits = c(finishssi-70, finishssi), breaks = seq(finishssi-70, finishssi, by = 10))

          }

          for (bot_loop in bot_teams) {
           #how many moves I need to do
            my_cyclers <- STG_CYCLER[TEAM_ID == bot_loop, CYCLER_ID]
            cyclers_left <- intersect(my_cyclers, in_game_cyclers)
            if (length(cyclers_left) > 0) {
                #even here I don't know the drawn cards
              hidden_information_output <- update_combinations_with_hidden_input(MIXED_STRATEGY$combinations, turn_start_deck,
                                                                                 team_id_input = bot_loop, pre_agg_no_list)

              bot_name <- bot_data[TEAM_ID == bot_loop, bot_name]
              bot_config <- NA

              funcargs <- list(hidden_information_output, deck_status,
                               bot_config, bot_loop, pre_agg_no_list)
            #  res_debug <- ttf_bot(hidden_information_output, deck_status,
                        #           bot_config, bot_loop)
              myfunc <- bot_name
              res <- do.call(myfunc, funcargs)
              if (bot_loop == 2) {
              print(res[1:10])
              }
          #    print(res)
              #   best_move_diff <- res[which.max(MOVE_ORDER_SCORE), MOVES]
              #   best_sof_diff <- res[which.max(SOF_SCORE), MOVES]
              # #  print(check_res2)
              #   if (best_move_diff != best_sof_diff) {
              #     browser()
              #   }
                #  scalued <- check_res2[ ,.(MOVES, EV = round(EV - max(EV), 2),
              moves <- EV_to_moves(res, deck_status)
              print(moves)
              for (loopperi in 1:nrow(moves)) {
                loop_cycler <- moves[loopperi, CYCLER_ID]
                loop_card <- moves[loopperi, CARD_ID]
                played_cards_data[TURN_ID == turn_id & CYCLER_ID == loop_cycler,  CARD_ID := loop_card]
              }


            }

          }

          print(zoom(game_status))

          print(deck_status[CYCLER_ID == 5 & Zone != "Removed"][order(Zone, MOVEMENT)])


          #for each bot






     # print(deck_status[CYCLER_ID %in% smart_cycler_ids  & Zone == "Hand", .(CARDS = paste0(sort(unique(MOVEMENT)), collapse = "-")), by = .(CYCLER_ID)][order(CYCLER_ID)])

      # if (second_cycler == 6 ) {
      #   print(paste0("rouler played: ", first_move))
      #   print(paste0("sprinteur played: ", second_move))
      # } else {
      #   print(paste0("rouler played: ", second_move))
      #   print(paste0("sprinteur played: ", first_move))
      # }







  #choose ai cycler cards
        ai_cyclers <- setdiff(in_game_cyclers, bots)
          for(loop in ai_cyclers) {
            card_id  <-  card_selector_by_stat(game_status, deck_status[CYCLER_ID == loop & Zone == "Hand"], loop, "SMART_MAX",  aim_downhill = TRUE)[, CARD_ID]
            played_cards_data[TURN_ID == turn_id & CYCLER_ID == loop,  CARD_ID := card_id]
          }






      for(loop_move in in_game_cyclers) {

        row_data <- played_cards_data[TURN_ID == turn_id & loop_move == CYCLER_ID]
        move_played <- STG_CARD[CARD_ID == row_data[, CARD_ID], MOVEMENT]
          deck_status <- play_card(cycler_id = loop_move,
                                   card_id = row_data[, CARD_ID],
                                   current_decks = deck_status, game_id, turn_id, con = NULL,
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

    # revert_turn <- 10
    # rev_cards <- played_cards_data[TURN_ID == revert_turn, .(CYCLER_ID, OPTIONS)]
    # for(cyclers_loop in nrow(rev_cards)) {
    #   opt_data <- data.table(CARD_ID = rev_cards[CYCLER_ID == cyclers_loop, OPTIONS[[1]]], OPTION = 1:4, CYCLER_ID = cyclers_loop)
    #   drawn_cards_revert <- deck_status[CYCLER_ID == cyclers_loop & Zone == "Deck"][opt_data, on = .(CYCLER_ID, CARD_ID)][, min(row_id), by = OPTION][, V1]
    #   deck_status[row_id %in% drawn_cards_revert, Zone := "Hand"]
    #
    # }
      #played_cards_data[TURN_ID == revert_turn, CARD_ID := NA]

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
    plot_winner <- total_winner[, mean(POSITION), by = CYCLER_ID][order(CYCLER_ID)]
    # print(ggplot(data=plot_winner, aes(x=CYCLER_ID,  y=V1, fill = as.factor(CYCLER_ID))) +
    #         #geom_line(linetype="dashed", color="blue", size=1.2)+
    #         geom_bar(stat="identity")+theme_minimal())
  #  total_winner[, .N, by = .(POSITION, CYCLER_ID)][order(CYCLER_ID)]
  # print(total_winner[, mean(POSITION), by = .(starting_lane, starting_row)][order(V1)])

    #print(full_action[, .(m)])
    print(total_winner[, mean(POSITION), by = CYCLER_ID][order(V1)])
   # print(total_winner[CYCLER_ID  %in% c(5,6)])
    #full_action[, .(SLIP = mean(SLIP), BLOCK = mean(BLOCK), EXHAUST = mean(EXHAUST), ASCEND = mean(ASCEND)), by = CYCLER_ID][order(CYCLER_ID)]
  #  print(full_action[, .(
   #                       SLIP = mean(SLIP_BONUS, na.rm = TRUE), BLOCK = mean(BLOCK_DIFF, na.rm = TRUE), EXHAUST = mean(EXHAUST, na.rm = TRUE),
  #                        ASCEND = mean(ASCEND_GAIN, na.rm = TRUE)), by = CYCLER_ID][order(CYCLER_ID)])

    turn_id <- 0
  }
#print(total_winner[, .N, by = .(CYCLER_ID, POSITION)])[order(CYCLER_ID, POSITION)]

