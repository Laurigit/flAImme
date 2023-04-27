  # required_data(c("STG_CYCLER", "STG_TRACK"))
#AWS docker IMAGE PITÄÄ REBUILDAA, ETTÄ TOIMII PILVESSÄ.
source("global.R")
#kaadu
global_dont_multicore <<- FALSE
con <- connDB(con, "flaimme")
   required_data(c("STG_TEAM","ADM_CYCLER_INFO", "ADM_CYCLER_DECK", "ADM_OPTIMAL_MOVES", "STG_TRACK", "SRC_TRACK", "SRC_TRACK_PIECE", "STG_TRACK_PIECE", "SRC_AI_CONF", "STG_AI_CONF", "ADM_AI_CONF"), force_update =TRUE)
  total_winner <- NULL
game_status_data <- list()
deck_status_data <- list()
turn_start_deck_status_data <- list()
  full_action <- NULL
  game_action <- NULL
move_diff_counter <- 0

  game_id <- 1
  #0 means use actual previous exh
  use_startup_exhaust <- 0
  start_format <-  "RANDOM"
  start_format <-  "REVERSED_BASED_ON_POS"
#  start_format <- "REVERSED"
plotting <- TRUE


  TIME_DATA <- data.table(GAME_ID = as.numeric(), TURN_ID = as.numeric(), PHASE = as.character(), DURATION = as.numeric())

 # bot_data <- data.table(bot_name = c("finish_rank_bot", "ttf_bot", "slots_over_bot"), TEAM_ID = c(2, 3, 4))
  #bot_data <- data.table(bot_name = c("next_turn_botti", "ruler_bot", "ttf_botti", "next_turn_botti"), TEAM_ID = c(1, 2 , 3, 4, 5))
 # bot_data <- data.table(bot_name = c("ruler_bot", "ttf_botti"), TEAM_ID = c(1 ,2))
  bot_data <- data.table(bot_name = c("ruler_bot_more", "ttf_botti_ignore_hidden_5", "ttf_botti_ignore_hidden_4", "ruler_bot", "ttf_botti_ignore_hidden_3", "ttf_botti_ignore_hidden_2"), TEAM_ID = c(1, 2, 3, 4, 5, 6))
 # bot_data <-  data.table(bot_name = NA, TEAM_ID = NA)
 # bot_data <- data.table(bot_name = c("relative_bot"), TEAM_ID = c(1))
  #bot_data <- data.table(bot_name = c("slots_over_bot"), TEAM_ID = c( 4))

  exh_left <- data.table(CYCLER_ID = as.numeric(), EXHAUST_LEFT = as.numeric())
  for (game_id in 1:200000) {
   # cycler_ids <- c(7, 4, 5, 6, 8, 1, 3, 2)
   # cycler_ids <- c(5, 6, 1, 2)
    turn_id <- 0
    tot_moni <- NULL
    monitor <- NULL
    repeat {
   #  browser()
    if (turn_id == 0) {
      TIME_DATA <-  time_phase("PRE", TRUE, TIME_DATA, game_id, turn_id)
    turn_game_status <- NULL
    deck_status_loop <- NULL
    turn_deck_start_loop <- NULL

  #  track <- sample(c(1,2,3,6,7,19,20,36,37,39,40,41,42),1)#as.integer(runif(1, 12, 17))
    track <- sample(c(12,13,14,15,16,17),1)#,20,36,37,39,40,41,42),1)#as.integer(runif(1, 12, 17))
    #track <- sample(c(1, 2, 3, 5, 6, 19, 20, 7, 36,37,39,40,41,42),1)#,20,36,37,39,40,41,42),1)#as.integer(runif(1, 12, 17))
    #track <- 12
    print(paste0("track_", track))

    #track 37 on mukula
    #ijk map explanation: i = starting point, k = movement, j = ending slot


    winner_state <- data.table(TURN_ID = numeric(), CYCLER_ID = numeric(), POSITION = numeric(), GAME_ID = numeric(),
                               row_over_finish = numeric(), finish_square = numeric())


   # cycler_vector <- c(1,2,3,4,5,6,7,8,9,10,11,12)
    cycler_vector <- c(1,2,3,4,5 ,6,7,8,9,10,11,12)
    # cycler_vector <- c(1,2,3,4)
    cycler_ids <- set_startup_formation(cycler_vector, start_format)

    #cycler_ids <- (c(1,2,3,4,5,6,7,8,9,10,11,12))
   # cycler_ids <- (c(1,2,3,4,5,6))
   # cycler_ids <- sample(c(5,6,7,8))
  #  cycler_ids <- sample(c(3,4,5,6))
    total_cyclers <- length(cycler_ids)
    game_status <- start_game(cycler_ids ,track, STG_TRACK_PIECE, STG_TRACK)
   # game_status <- randomize_start_grid(game_status)

    #initial decks
    deck_status <- create_decks(cycler_ids, ADM_CYCLER_DECK)
    #add exhaust
    if (use_startup_exhaust == 0) {
      if (game_id %% 5 == 0) {
        use_startup_exhaust_mod <- -1
      } else {
        use_startup_exhaust_mod <- 0
      }
    }


    ## tää on nyt säädetty niin, että joka 5 nollataan.
    if (nrow(exh_left) > 0 & use_startup_exhaust_mod == 0) {
#0 means use actual previous exh
    deck_status <- add_startup_exhaustion(exh_left, deck_status)
    print(deck_status[CARD_ID == 1, .(EXHAUST = .N), by = CYCLER_ID][order(CYCLER_ID)])
    } else if (use_startup_exhaust_mod > 0) {
      custom_exh <- data.table(CYCLER_ID = cycler_ids, EXHAUST_LEFT = use_startup_exhaust * 2)
      deck_status <- add_startup_exhaustion(custom_exh, deck_status)
    }

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

    starting_cyclers <- cycler_ids





    con <- connDB(con, "flaimme")
    #required_data("ADM_OPTIMAL_MOVES", force_update = TRUE)


    track_lefter_select <- paste0('"', pre_aggr_game_status$aggr_to_slots[TRACK_LEFT != "", paste0(TRACK_LEFT, collapse = '", "')], '"')
    ADM_OPTIMAL_MOVES <- data.table(dbGetQuery(con, paste0('SELECT TRACK_LEFT, DECK_LEFT, TURNS_TO_FINISH, DRAW_ODDS, SLOTS_OVER_FINISH, NEXT_MOVE, FOLLOWING_MOVE,
                      PRIORITY FROM ADM_OPTIMAL_MOVES WHERE TRACK_LEFT IN (', track_lefter_select, ')')))



    ADM_OPTIMAL_MOVES_AGGR <- ADM_OPTIMAL_MOVES[, .(

                                                    TURNS_TO_FINISH  = TURNS_TO_FINISH [which.min(PRIORITY)],

                                                    SLOTS_OVER_FINISH = SLOTS_OVER_FINISH[which.min(PRIORITY)],
                                                    NEXT_MOVE = NEXT_MOVE[which.min(PRIORITY)],
                                                    FOLLOWING_MOVE = FOLLOWING_MOVE[which.min(PRIORITY)]),

                                                by = .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS)]
    TIME_DATA <-  time_phase("PRE", FALSE, TIME_DATA, game_id, turn_id)
    turn_id <- 1
    alotus <- Sys.time()

    #SIMULATE BREAKAWAY

    deck_status <- simulate_breakaway_return_new_deck(game_status, deck_status, 5)
    bet_winners <- deck_status[Zone == "Removed", .N, CYCLER_ID]
    betted_cards <- deck_status[Zone == "Removed", .(TOT_BET = sum(MOVEMENT)), by = CYCLER_ID]
    start_pos <- game_status[CYCLER_ID %in% bet_winners[, CYCLER_ID], .(ORIG_START_POS = GAME_SLOT_ID), by = CYCLER_ID]
    start_pos[, EXTRA_MOVEMENT := 10 - ORIG_START_POS]
    joinaa_bw <- betted_cards[start_pos, on = "CYCLER_ID"]
    joinaa_bw[, OVER_BET := TOT_BET - EXTRA_MOVEMENT]
    lane_counter <- 0
    print(betted_cards)
    for (bet_winner_loop in bet_winners[, CYCLER_ID]) {
      lane_counter <- lane_counter + 1

      game_status<- set_cycler_position(bet_winner_loop, 10, lane_counter, game_status)
    }

    print(zoom(game_status))
    }
      TIME_DATA <-  time_phase("TURN", TRUE, TIME_DATA, game_id, turn_id)
     # print(turn_id)

      in_game_cyclers <- game_status[order(-SQUARE_ID)][CYCLER_ID > 0, CYCLER_ID]
      finished_cyclers <- setdiff(starting_cyclers, in_game_cyclers)







      #for each player, choose cycler, draw cards
      #EVERYBODY DRAWS
      ##take copy of deck before drawing for calculation which cycler to play first
      turn_start_deck <- copy(deck_status)

      for (loop_cycler in in_game_cyclers) {
        deck_status <- draw_cards(loop_cycler, deck_status, 4, con = NULL,  turn_id = turn_id, game_id = game_id)

        # if (loop_cycler %in% c(3, 4)) {
        #   deck_status[CYCLER_ID == loop_cycler & Zone != "Removed", Zone := "Hand" ]
        # }

        played_cards_data[CYCLER_ID == loop_cycler & TURN_ID == turn_id,
                          OPTIONS := list(list(deck_status[CYCLER_ID == loop_cycler & Zone == "Hand", CARD_ID]))]
      }

      turn_game_status[[turn_id]] <- copy(game_status)
      deck_status_loop[[turn_id]] <- copy(deck_status)
      turn_deck_start_loop[[turn_id]] <- copy(turn_start_deck)

#       turn_id <- 8
#       deck_status <- deck_status_loop[[turn_id]]
#       game_status <- turn_game_status[[turn_id]]
#       turn_start_deck <- turn_deck_start_loop[[turn_id]]
#       played_cards_data[TURN_ID > turn_id, OPTIONS := ""]
#       played_cards_data[TURN_ID >= turn_id, CARD_ID := 0]
# zoom(game_status)


#if (turn_id == 6) {browser()}



          bot_teams <- bot_data[, TEAM_ID]
          bots <- STG_CYCLER[TEAM_ID %in% bot_teams, CYCLER_ID]
          bots_left_total <- intersect(bots, in_game_cyclers)
          #any bots left?
          if (length(bots_left_total) > 0) {
        #    calc_ttf_input_all <- ifelse(turn_id >= 8, 0, turn_id)
            calc_ttf_input_all <- 0
           # calc_ttf_input <- ifelse(turn_id >= 5, 0, turn_id)
            pre_agg_no_list <- pre_aggr_game_status$aggr_to_slots

          #  input_case_count <- 100
          #  input_case_count <- NULL
            input_case_count <- 3000 * pmax((turn_id - 4), 1) ^ (4 / 5)
            print(paste0("input case count: ", input_case_count))
            TIME_DATA <-  time_phase("COMB", TRUE, TIME_DATA, game_id, turn_id)
              combinations_output <- calc_combinations_data(con, game_status, turn_start_deck, deck_status,
                                                            pre_agg_no_list, matr_ijk, reverse_slots_squares, slip_map_matrix, STG_CYCLER,
                                                            calc_ttf = calc_ttf_input_all, case_count = input_case_count,
                                                            hidden_info_teams = c(1,4), input_turn_id = turn_id,
                                                            finished_cyclers)
              TIME_DATA <-  time_phase("COMB", FALSE, TIME_DATA, game_id, turn_id)
              TIME_DATA <-  time_phase("MIX", TRUE, TIME_DATA, game_id, turn_id)
             MIXED_STRATEGY <- calculate_mixed_strategy(combinations_output, consensus_config_id = NA, turn_start_deck)
             TIME_DATA <-  time_phase("MIX", FALSE, TIME_DATA, game_id, turn_id)

             if (plotting == TRUE) {
 ss_for_picture <- MIXED_STRATEGY$combinations[, .(FINISH_RANK_ALL, FINISH_ESTIMATE_MEAN , CYCLER_ID, PROB_PRODUCT, case_id, NEW_GAME_SLOT_ID, CASE_PROB )]
# ss_for_picture[, CASE_PROB := prod(PROB_PRODUCT^(1/2)), by = case_id]

 aggr_pic <- ss_for_picture[, .(FINISH_RANK_EST = mean(FINISH_RANK_ALL), FINISH_ESTIMATE_MEAN = sum(FINISH_ESTIMATE_MEAN * CASE_PROB) / sum(CASE_PROB),
                                NEW_GAME_SLOT_ID = sum(NEW_GAME_SLOT_ID  * CASE_PROB) / sum(CASE_PROB)), by = CYCLER_ID]

 aggr_pic[, TURN_ID := turn_id]
 if (turn_id == 1) {
   acnhor_turn <- aggr_pic[, min(FINISH_ESTIMATE_MEAN)]
 }
 aggr_pic[, TTF_SCALED := FINISH_ESTIMATE_MEAN - acnhor_turn + TURN_ID - 1, by = .(TURN_ID)]
 aggr_pic[, CYC_TYPE := CYCLER_ID  %% 2]

 TTF_stats <- rbind(TTF_stats, aggr_pic)
 finishssi <- game_status[FINISH == 1, min(GAME_SLOT_ID)]+10
 startti <-game_status[START == 1, min(GAME_SLOT_ID)] + 2
 suppressWarnings(print(ggplot(data=TTF_stats, aes(x=NEW_GAME_SLOT_ID, y=TTF_SCALED , group=CYCLER_ID)) +
         #geom_line(linetype="dashed", color="blue", size=1.2)+
         geom_line(size=1.5, aes(color=as.factor(CYCLER_ID)))+
         geom_point(size = 4, aes(color=as.factor(CYCLER_ID), shape=as.factor(CYC_TYPE))) +
         geom_vline(
           xintercept = finishssi,
           na.rm = FALSE,
           show.legend = NA
         ) +
          scale_color_manual(values=c("red", "red", "blue", "blue", "black", "black", "green", "green", "white", "white", "purple", "purple")) +
        scale_y_continuous(limits = c(-3, 4), breaks = c(-3:4)) + scale_x_continuous(limits = c(1, finishssi), breaks = seq(1, finishssi, by = 10))))
#xlim(0, finishssi) + ylim(1, 13) +

             }
           }
          #linetype = "solid",
          TIME_DATA <-  time_phase("BOTS", TRUE, TIME_DATA, game_id, turn_id)
          for (bot_loop in bot_teams) {
           #how many moves I need to do
          #  print(paste0("bot_", bot_loop))
            my_cyclers <- STG_CYCLER[TEAM_ID == bot_loop, CYCLER_ID]
            cyclers_left <- intersect(my_cyclers, in_game_cyclers)
            if (length(cyclers_left) > 0) {
                #even here I don't know the drawn cards





              hidden_information_output <- MIXED_STRATEGY$combinations
              bot_name <- bot_data[TEAM_ID == bot_loop, bot_name]
              bot_config <- NA

              funcargs <- list(hidden_information_output, deck_status,
                               bot_config, bot_loop, pre_agg_no_list, turn_id)
              if (bot_loop %in% c(99)) {

                added_next_move <- add_next_move_calc(hidden_information_output, matr_ijk, reverse_slots_squares, slip_map_matrix,
                                                     pre_agg_no_list, STG_CYCLER, my_team = bot_loop)

                funcargs <- list(added_next_move, deck_status,
                                 bot_config, bot_loop, pre_agg_no_list, turn_id)
              }
              if (bot_loop %in% c(99)) {

                res_debug_hidden <- ttf_botti_ignore_hidden(hidden_information_output, deck_status,
                                                              bot_config, bot_loop, pre_agg_no_list, turn_id)
                moves_hidden <- EV_to_moves(res_debug_hidden, deck_status, turn_start_deck)

                }


#
# #
             # res_debug <- ttf_botti_ignore_hidden_5(hidden_information_output, deck_status,
             #          bot_config, bot_loop, pre_agg_no_list, turn_id)

              myfunc <- bot_name
                res <- do.call(myfunc, funcargs)



              #

          #    print(res)
              #   best_move_diff <- res[which.max(MOVE_ORDER_SCORE), MOVES]
              #   best_sof_diff <- res[which.max(SOF_SCORE), MOVES]
              # #  print(check_res2)
              #   if (best_move_diff != best_sof_diff) {
              #     browser()
              #   }
                #  scalued <- check_res2[ ,.(MOVES, EV = round(EV - max(EV), 2),
              moves <- EV_to_moves(res, deck_status, turn_start_deck)

              if (bot_loop == 99) {
              #  browser()
             #   print(res[1:min(10, nrow(res)), .(MOVES, EV, MOVE_DIFF_SCORE, EXHAUST_SCORE, TTF_SCORE, TURNS_TO_FINISH, FINISH_RANK_SCORE, MOVE_ORDER_SCORE, OVER_FINISH_SCORE)], row.names = FALSE)#[draw_odds_C1 > 0 & draw_odds_C2 > 0])
              #print(res_debug_hidden[1:min(6, nrow(res_debug_hidden)), .(MOVES, EV, MOVE_DIFF_SCORE, EXHAUST_SCORE, TTF_SCORE, TURNS_TO_FINISH, FINISH_RANK_SCORE, MOVE_ORDER_SCORE, OVER_FINISH_SCORE)], row.names = FALSE)#[draw_odds_C1 > 0 & draw_odds_C2 > 0])

              options_dt <- deck_status[CYCLER_ID %in% c(7, 8) & Zone == "Deck", .N, by = .(CYCLER_ID, MOVEMENT)][order(CYCLER_ID, MOVEMENT)]
              options_dt[, N := ifelse(is.na(N), 0, N)]
              if (nrow(options_dt)> 0) {
              options_dcast_deck <- dcast.data.table(options_dt, CYCLER_ID ~ MOVEMENT, value.var = "N")
          #    print(options_dcast_deck)
              options_dt[, N := ifelse(is.na(N), 0, N)]
              }
                options_dt <- deck_status[CYCLER_ID %in% c(7, 8) & Zone == "Hand", .N, by = .(CYCLER_ID, MOVEMENT)][order(CYCLER_ID, MOVEMENT)]
                options_dcast <- dcast.data.table(options_dt, CYCLER_ID ~ MOVEMENT, value.var = "N")
                print(options_dcast)

                print(zoom(game_status))

                print(moves[order(CYCLER_ID)])
                moves4 <- moves

               sort1 <- moves4[order(CYCLER_ID), .(CYCLER_ID, CARD_ID)]
               sort2 <- moves_hidden[order(CYCLER_ID), .(CYCLER_ID, CARD_ID)]

               if (all.equal(sort1, sort2) != TRUE) {
                 print("######################################################################")
                 print("actually played")
                 print(sort1)
                 print("played without hidden info")
                 print(sort2)
                 move_diff_counter <- c(move_diff_counter, track)
                 dt_t <- data.table(TRACK = move_diff_counter)
                 #dt_t[, .N, by = TRACK]
               }

              }

              for (loopperi in 1:nrow(moves)) {
                loop_cycler <- moves[loopperi, CYCLER_ID]
                loop_movement <- max(moves[loopperi, CARD_ID], 2)
                loop_card <- select_min_card_value_with_same_movement(loop_cycler, loop_movement, deck_status[CYCLER_ID == loop_cycler & Zone == "Hand"], game_status)

                played_cards_data[TURN_ID == turn_id & CYCLER_ID == loop_cycler,  CARD_ID := loop_card]
              }




            }
          }
          TIME_DATA <-  time_phase("BOTS", FALSE, TIME_DATA, game_id, turn_id)

#           monitor[, TURN_ID := turn_id]
# tot_moni <- rbind(tot_moni, monitor)
# max_non_over_finish <- tot_moni[variable != "OVER_FINISH_SCORE", max(value)]

#   print(ggplot(data=tot_moni, aes(x=TURN_ID, y=value, group=variable))+
#           geom_line(linetype="dashed", color="blue", size=1.2) +
#            geom_line(size=1.5, aes(linetype = "solid", color=as.factor(variable)))+
# #           geom_point(size = 3, aes(color=as.factor(CYCLER_ID), shape=as.factor(CYC_TYPE))) +
# #            scale_color_manual(values=c("red", "red", "blue", "blue", "black", "black", "green", "green")) +
#            xlim(1, turn_id) + ylim(0, max_non_over_finish)) + scale_x_continuous(limits = c(0, turn_id), breaks = seq(0, turn_id, by = 1))
# #

         # print(deck_status[CYCLER_ID == 1 & Zone != "Removed"][order(Zone, MOVEMENT)])


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
          deck_status <- suppressWarnings(play_card(cycler_id = loop_move,
                                   card_id = row_data[, CARD_ID],
                                   current_decks = deck_status, game_id, turn_id, con = NULL,
                                   force = TRUE,
                                   copy = FALSE))

          game_status <- move_cycler(game_status, loop_move, move_played, slipstream = FALSE,
                                     ignore_block = FALSE,
                                     ignore_end_of_track = FALSE,
                                     return_numeric_position = FALSE)

      }

      #SLIP STREAM
      game_status <- apply_slipstream(game_status)

     #EXHAUST
      max_old_rid <- deck_status[, max(row_id)]
      deck_status <- apply_exhaustion(deck_status, game_status)


    #  print(deck_status[row_id > max_old_rid, .N, by = CYCLER_ID][order(CYCLER_ID)])

      #RECROD WINNER
      winner_state <- check_winner(game_status, winner_state, turn_id, game_id)

      #remove finished
      game_status <- clear_finishers(game_status,winner_state[, CYCLER_ID])
      #update who is left
      cycler_ids <- game_status[CYCLER_ID >0, CYCLER_ID]

      #add exhaust if no cards left
      deck_status <-  add_exhaust_if_no_cards_left(deck_status, cycler_ids)

      # if (nrow(winner_state) > 0) {
      #   break()
      # }
      if (length(cycler_ids) == 0) {

        exh_left <- deck_status[CARD_ID == 1 & Zone != "Removed", .(EXHAUST_LEFT = .N), by = CYCLER_ID]


        break()
      }

      TIME_DATA <-  time_phase("TURN", FALSE, TIME_DATA, game_id, turn_id)

   turn_id <- turn_id + 1
       print(turn_id)


     }

    game_status_data[[game_id]] <- turn_game_status
    deck_status_data[[game_id]] <- deck_status_loop
    turn_start_deck_status_data[[game_id]] <- turn_deck_start_loop
  # turn_game_status <-    game_status_data[[3]]
  #  deck_status_loop <-    deck_status_data[[3]]
  #  turn_deck_start_loop <-    turn_start_deck_status_data[[3]]

    #print(winner_state)
#    print(TIME_DATA[DURATION < 1000, .(DUR = mean(DURATION)), by = .(TURN_ID, PHASE)][order(PHASE, TURN_ID)])
 #   print(TIME_DATA[DURATION < 1000 & PHASE == "TURN", .(DUR = sum(DURATION)), by = .(GAME_ID, PHASE)][order(PHASE)])

    print(winner_state)

  #  ssstartPos <- used_startup_data[,. (CYCLER_ID, starting_lane, starting_row)]
  #  join_sinner_pos <- ssstartPos[winner_state, on = "CYCLER_ID"]
  #  full_action <- rbind(full_action, action_data)
    copy_winner <- winner_state
    copy_winner[, TRACK_ID := track]
    #copy_winner[, start_order := list(list(starting_cyclers))]
    copy_winner[, START_POSITION := match(CYCLER_ID, starting_cyclers)]
    join_bw_to_winner <- joinaa_bw[copy_winner, on = "CYCLER_ID"]
    total_winner <- rbind(total_winner, join_bw_to_winner)

#browser()
    plot_winner <- total_winner[, mean(POSITION), by = CYCLER_ID][order(CYCLER_ID)]
    # print(ggplot(data=plot_winner, aes(x=CYCLER_ID,  y=V1, fill = as.factor(CYCLER_ID))) +
    #         #geom_line(linetype="dashed", color="blue", size=1.2)+
    #         geom_bar(stat="identity")+theme_minimal())
  #  total_winner[, .N, by = .(POSITION, CYCLER_ID)][order(CYCLER_ID)]
  # print(total_winner[, mean(POSITION), by = .(starting_lane, starting_row)][order(V1)])

    #print(full_action[, .(m)])
    print(total_winner[, mean(POSITION), by = CYCLER_ID][order(V1)])
    print(total_winner[, mean(POSITION), by = START_POSITION][order(V1)])
    print(total_winner[, mean(POSITION), by = OVER_BET][order(V1)])

    # print(total_winner[, .N, by = .(CYCLER_ID, POSITION)][order(CYCLER_ID)], row.names = FALSE)

    # print(total_winner[CYCLER_ID  %in% c(5,6)])
    #full_action[, .(SLIP = mean(SLIP), BLOCK = mean(BLOCK), EXHAUST = mean(EXHAUST), ASCEND = mean(ASCEND)), by = CYCLER_ID][order(CYCLER_ID)]
  #  print(full_action[, .(
   #                       SLIP = mean(SLIP_ttONUS, na.rm = TRUE), BLOCK = mean(BLOCK_DIFF, na.rm = TRUE), EXHAUST = mean(EXHAUST, na.rm = TRUE),
  #                        ASCEND = mean(ASCEND_GAIN, na.rm = TRUE)), by = CYCLER_ID][order(CYCLER_ID)])

    turn_id <- 0
  }
#print(total_winner[, .N, by = .(CYCLER_ID, POSITION)])[order(CYCLER_ID, POSITION)]

