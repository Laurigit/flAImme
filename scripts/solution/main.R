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

  playing_teams <- c(1, 2, 3, 4)
 # playing_teams <- c(1, 2, 3)

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
    track <- 13#as.integer(runif(1, 12, 17))
    #ijk map explanation: i = starting point, k = movement, j = ending slot
    ijk <- ijk_map(track, STG_TRACK_PIECE, STG_TRACK)

    slip_map <- slipstream_map(track, STG_TRACK_PIECE, STG_TRACK)
    slip_map_matrix <- as.matrix(slip_map)
   # reverse_slip_map <- slip_map_matrix[nrow(slip_map_matrix):1,]

    winner_state <- data.table(TURN_ID = numeric(), CYCLER_ID = numeric(), POSITION = numeric(), GAME_ID = numeric(),
                               row_over_finish = numeric(), finish_square = numeric())


    cycler_ids <- sample(c(1,2,3,4,5,6,7,8))
  #  cycler_ids <- sample(c(3,4,5,6))
    total_cyclers <- length(cycler_ids)
    game_status <- start_game(cycler_ids ,track, STG_TRACK_PIECE, STG_TRACK)
   # game_status <- randomize_start_grid(game_status)

    #initial decks
    deck_status <- create_decks(cycler_ids, ADM_CYCLER_DECK)




   #prepare log data
    move_data <- data.table(CYCLER_ID = cycler_ids, MOVEMENT = 0, CARD_ID = 0, OPTIONS = list(), key_col = "1")
    played_cards_data <- merge(x = data.table(TURN_ID = 1:25, key_col = "1"), y = move_data, by = "key_col", all = TRUE, allow.cartesian = TRUE)
    played_cards_data[, key_col := NULL]


    TTF_stats <- NULL
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
      turn_start_time <- Sys.time()
      in_game_cyclers <- game_status[order(-SQUARE_ID)][CYCLER_ID > 0, CYCLER_ID]

      ai_cyclers <- setdiff(cycler_ids, smart_cycler_ids)
      in_game_smart_cyclers <- setdiff(in_game_cyclers, ai_cyclers)

      #for each player, choose cycler, draw cards
      #EVERYBODY DRAWS
      ##take copy of deck before drawing for calculation which cycler to play first
      turn_start_deck <- copy(deck_status)
      for (loop_cycler in cycler_ids) {
        deck_status <- draw_cards(loop_cycler, deck_status, 4, con = con,  turn_id = turn_id, game_id = game_id)
        played_cards_data[CYCLER_ID == loop_cycler & TURN_ID == turn_id,
                          OPTIONS := list(list(deck_status[CYCLER_ID == loop_cycler & Zone == "Hand", CARD_ID]))]
      }



      #CTM handling.
      # load from DB
      # if playing with humans, dont update DB
      # update only new opt results in memory that have been generated during the game

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
          if (turn_id >= 1) {
            calc_ttf <- TRUE
          } else {
            calc_ttf <- FALSE
          }
          rm("ADM_OPTIMAL_MOVES")
          required_data("ADM_OPTIMAL_MOVES")
          ctM_res <- cyclers_turns_MOVEMEMENT_combs(con, ADM_OPTIMAL_MOVES, game_status, deck_status, pre_aggr_game_status, calc_ttf)
          ctM_data <- ctM_res$ctM_data

          min_ttf <- ctM_data[, min(TURNS_TO_FINISH)]
          if (calc_ttf == FALSE) {
          own_TTF <- cyclers_turns_MOVEMEMENT_combs(con, ADM_OPTIMAL_MOVES, game_status,
                                                    deck_status, pre_aggr_game_status, TRUE, calc_draw_odds = TRUE, subset_cyclers = smart_cycler_ids)
          own_TTF_data <- own_TTF$ctM_data[, .(MY_TURNS_TO_FINISH = TURNS_TO_FINISH, CYCLER_ID, MOVEMENT, new_slot_after_moving, DRAW_ODDS, DECK_LEFT)]
          min_ttf <- own_TTF_data[, min(MY_TURNS_TO_FINISH)]
          }
          # ss_ttf <- own_TTF_data[, .(min_ttf = min(MY_TURNS_TO_FINISH)), by = .(CYCLER_ID)]
          # ss_ttf[, TURN_ID := turn_id]
          # TTF_stats <- rbind(TTF_stats, ss_ttf)
          # # ggplot(data=TTF_stats, aes(x=TURN_ID, y=min_ttf, group=CYCLER_ID)) +
          # #   geom_line()+
          # #   geom_point()
          # # # Change line types
          # print(ggplot(data=TTF_stats, aes(x=TURN_ID, y=min_ttf, group=CYCLER_ID)) +
          #   #geom_line(linetype="dashed", color="blue", size=1.2)+
          #     geom_line(aes(linetype=as.factor(CYCLER_ID) , color=as.factor(CYCLER_ID)))+
          #   geom_point(aes(linetype=as.factor(CYCLER_ID), size=2, color=as.factor(CYCLER_ID))) +
          #   xlim(0, 16) + ylim(0, 14))






          #calculate mixed strategies
          rm("ADM_OPTIMAL_MOVES")
          required_data("ADM_OPTIMAL_MOVES")
          MIXED_STRATEGY_all <- calculate_mixed_strategy(game_status, deck_status, ijk,
                                                         slip_map_matrix, ADM_AI_CONF, ADM_OPTIMAL_MOVES, ctM_data, STG_TEAM, TRUE, min_ttf)
          MIXED_STRATEGY <- MIXED_STRATEGY_all$opponent_strat
          combinations_with_p <- MIXED_STRATEGY_all$combinations
          if (calc_ttf == FALSE) {
          only_my_ttf <-   own_TTF_data[, .(MY_TURNS_TO_FINISH, CYCLER_ID, MOVEMENT, new_slot_after_moving)]

          join_my_ttf <- only_my_ttf[combinations_with_p, on = .(CYCLER_ID, MOVEMENT, new_slot_after_moving == NEW_GAME_SLOT_ID)]


          thinking_time_so_far <- difftime(Sys.time(), turn_start_time, units = c("secs"))
          draw_odds_and_deck_left <-   own_TTF_data[, .(CYCLER_ID, MOVEMENT, DRAW_ODDS, DECK_LEFT)]
          join_draw_odds <- draw_odds_and_deck_left[join_my_ttf, on = .(CYCLER_ID, MOVEMENT)]
          count_missing <- join_draw_odds[is.na(MY_TURNS_TO_FINISH) & CYCLER_ID %in% smart_cycler_ids][, .N]
         # if (as.numeric(thinking_time_so_far) <= 60 & count_missing > 0) {
          if ( count_missing > 0) {

            join_draw_odds[is.na(MY_TURNS_TO_FINISH) & CYCLER_ID %in% smart_cycler_ids,
                           recalc_ttf := as.double(finish_turns_db(con, TRACK_LEFT, DECK_LEFT, pre_aggr_game_status, new_slot_after_moving,
                                                                                        draw_odds_raw_data = DRAW_ODDS, save_to_DB = TRUE)),
                           by = .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS, new_slot_after_moving)]
            join_draw_odds[is.na(MY_TURNS_TO_FINISH), MY_TURNS_TO_FINISH := recalc_ttf]
            join_draw_odds[!is.na(MY_TURNS_TO_FINISH) %in% smart_cycler_ids]
            join_my_ttf <- copy(join_draw_odds)
            join_my_ttf[, TURNS_TO_FINISH := ifelse(!is.na(MY_TURNS_TO_FINISH), MY_TURNS_TO_FINISH, TURNS_TO_FINISH)]
            setnames(join_my_ttf, "new_slot_after_moving", "NEW_GAME_SLOT_ID")
          }
          } else {
            join_my_ttf <- MIXED_STRATEGY_all$combinations
          }

          #calc my ev given opponent strategies
          finish_slot <- pre_aggr_game_status$aggr_to_slots[FINISH == 1, GAME_SLOT_ID]
          join_my_ttf[, OVER_FINISH := pmax(NEW_GAME_SLOT_ID - finish_slot, 0)]
          #setkeyv sorts cyclers by case_id and new_square
          setorder(join_my_ttf, case_id, -NEW_SQUARE)
         # join_my_ttf[, ':=' (MY_TEAM_ROW = ifelse(CYCLER_ID %in% smart_cycler_ids, 1,NA))]
         # join_my_ttf[, ':=' (MY_TEAM_MIN_TTF = min(TURNS_TO_FINISH * MY_TEAM_ROW, na.rm = TRUE)), by = case_id]
          #-1 so that we include cases where we push opponent back
         # join_my_ttf[, ':=' (RELEVANT_OPPONENT = MY_TEAM_MIN_TTF >= (TURNS_TO_FINISH - 1)), by = case_id]
          join_my_ttf[, ':=' (CYCLER_MEAN_TTF = mean(TURNS_TO_FINISH)), by = .(CYCLER_ID)]
          nemesis <- join_my_ttf[!CYCLER_ID %in% smart_cycler_ids, CYCLER_ID[which.min(CYCLER_MEAN_TTF)]]
          if (length(nemesis) == 0) {nemesis <- min(smart_cycler_ids)}
          join_my_ttf[, ':=' (NEMESIS_ROW = ifelse(CYCLER_ID %in% nemesis, 1, NA))]
          join_my_ttf[, ':=' (NEMESIS_TTF = min(TURNS_TO_FINISH * NEMESIS_ROW, na.rm = TRUE)), by = case_id]

        #  join_my_ttf[, ':=' (TTF_DIFF_OF_MEAN = CYCLER_MEAN_TTF - TURNS_TO_FINISH)]
        #  join_track_left[, ':=' (TEAM_TTF_MEAN = mean(TURNS_TO_FINISH), cycs = .N), by = .(case_id, TEAM_ID)]
       #   join_my_ttf[, ':=' (CASE_DIFF = sum(TTF_DIFF_OF_MEAN * RELEVANT_OPPONENT), tot_cycs = sum(RELEVANT_OPPONENT)), by = .(case_id)]
        #  join_my_ttf[, ':=' (COMPETITOR_AVG_TTF = (CASE_DIFF - TTF_DIFF_OF_MEAN) / (tot_cycs - 1))]
          join_my_ttf[, ':='(RELATIVE_TTF = (NEMESIS_TTF - TURNS_TO_FINISH))]#positive is good
          join_my_ttf[, ':=' (SLOTS_PROGRESSED = NEW_GAME_SLOT_ID - curr_pos)]
          join_my_ttf[, ':=' (MOVE_ORDER = seq_len(.N),
            MOVE_DIFF_RELATIVE = MOVE_DIFF - (sum(MOVE_DIFF) - MOVE_DIFF) / (.N - 1)
          ), by = case_id]



          # join_track_left[, MOVE_ORDER := seq_len(.N), by = case_id]
          setkeyv(join_my_ttf, cols = c("case_id", "TEAM_ID", "CYCLER_ID"))

          # join_track_left[, MOVE_DIFF_RELATIVE := MOVE_DIFF - (sum(MOVE_DIFF) - MOVE_DIFF) / (.N - 1), by = case_id]

          join_my_ttf[, ':=' (MOVE_DIFF_SCORE = MOVE_DIFF_RELATIVE * 0.25 * pmax(min_ttf - 4, 0.1),
                                  EXHAUST_SCORE = ((1 + MOVE_ORDER) / total_cyclers) * EXHAUST * pmax(min_ttf - 3, 0) ^ 1.2 * -0.2,
                               #   TTF_SCORE = RELATIVE_TTF * 20 * ((total_cyclers - max(MOVE_ORDER, 4)) / total_cyclers),
                              TTF_SCORE = RELATIVE_TTF * 5,
                                  # CYC_DIST_SCORE = DIST_TO_TEAM * - 0.03 * pmax(TURNS_TO_FINISH - 3, 0),
                                  MOVE_ORDER_SCORE = - MOVE_ORDER * 0.015 * (22 - min_ttf),
                                  OVER_FINISH_SCORE = OVER_FINISH * 100,
                                  SLOTS_PROGRESSED_SCORE = SLOTS_PROGRESSED * 0.001 * (16 - min_ttf))]

          join_my_ttf[, TOT_SCORE := (MOVE_DIFF_SCORE +
                                            EXHAUST_SCORE +
                                             TTF_SCORE +
                                            #   CYC_DIST_SCORE +
                                            SLOTS_PROGRESSED_SCORE +
                                            MOVE_ORDER_SCORE +
                                            OVER_FINISH_SCORE)]

          #join_opponent strat
          ss_strat <- MIXED_STRATEGY[, .(MOVES, TEAM_ID, PROB_PRODUCT)]
          join_strat <- ss_strat[join_my_ttf, on = .(MOVES, TEAM_ID)]

          ss_for_picture <- join_strat[, .(TURNS_TO_FINISH, CYCLER_ID, PROB_PRODUCT, case_id, NEW_GAME_SLOT_ID)]
          ss_for_picture[, CASE_PROB := prod(PROB_PRODUCT^(1/2)), by = case_id]

          aggr_pic <- ss_for_picture[, .(TURNS_TO_FINISH = sum(TURNS_TO_FINISH * CASE_PROB) / sum(CASE_PROB),
                                         NEW_GAME_SLOT_ID = sum(NEW_GAME_SLOT_ID  * CASE_PROB) / sum(CASE_PROB)), by = CYCLER_ID]

          aggr_pic[, TURN_ID := turn_id]
          if (turn_id == 1) {
          acnhor_turn <- aggr_pic[, min(TURNS_TO_FINISH)]
          }
          aggr_pic[, TTF_SCALED := TURNS_TO_FINISH - acnhor_turn + TURN_ID - 1, by = .(TURN_ID)]
          TTF_stats <- rbind(TTF_stats, aggr_pic)
          print(ggplot(data=TTF_stats, aes(x=NEW_GAME_SLOT_ID, y=TTF_SCALED, group=CYCLER_ID)) +
                  #geom_line(linetype="dashed", color="blue", size=1.2)+
                  geom_line(aes(linetype=as.factor(CYCLER_ID) , color=as.factor(CYCLER_ID)))+
                  geom_point(aes(linetype=as.factor(CYCLER_ID), color=as.factor(CYCLER_ID))) +
                  xlim(6, 78) + ylim(-1, 1.5))



          #weight my cyclers based on ttf
          join_strat[, CYCLER_WEIGHT := (1 - CYCLER_MEAN_TTF / sum(CYCLER_MEAN_TTF )), by = .(TEAM_ID, case_id)]

          check_res <- join_strat[, .(TEAM_SCORE = sum(TOT_SCORE * CYCLER_WEIGHT),
                                           MOVE_DIFF_SCORE  = sum(MOVE_DIFF_SCORE * CYCLER_WEIGHT),
                                           EXHAUST_SCORE = sum(EXHAUST_SCORE * CYCLER_WEIGHT),
                                      SLOTS_PROGRESSED_SCORE = sum(SLOTS_PROGRESSED_SCORE * CYCLER_WEIGHT),
                                           TTF_SCORE = sum(TTF_SCORE * CYCLER_WEIGHT),
                                            TURNS_TO_FINISH = min(TURNS_TO_FINISH),
                                           #     CYC_DIST_SCORE = sum(CYC_DIST_SCORE),
                                           MOVE_ORDER_SCORE = sum(MOVE_ORDER_SCORE * CYCLER_WEIGHT),
                                           OVER_FINISH_SCORE = sum(OVER_FINISH_SCORE * CYCLER_WEIGHT)), by = .(MOVES, TEAM_ID, case_id, PROB_PRODUCT, CYCLERS)]

          check_res[, OPPONENT_CASE_PROB := prod(PROB_PRODUCT) / PROB_PRODUCT, by = case_id]
          check_res[, OPPONENT_CASE_PROB := ifelse(is.na(OPPONENT_CASE_PROB), 0.000001, OPPONENT_CASE_PROB)]


          check_res2 <- check_res[TEAM_ID %in% smart_team, .(EV = sum(TEAM_SCORE * OPPONENT_CASE_PROB) / sum(OPPONENT_CASE_PROB),
                                      MOVE_DIFF_SCORE  = sum(MOVE_DIFF_SCORE * OPPONENT_CASE_PROB) / sum(OPPONENT_CASE_PROB),
                                      EXHAUST_SCORE = sum(EXHAUST_SCORE * OPPONENT_CASE_PROB) / sum(OPPONENT_CASE_PROB),
                                      SLOTS_PROGRESSED_SCORE = sum(SLOTS_PROGRESSED_SCORE * OPPONENT_CASE_PROB) / sum(OPPONENT_CASE_PROB),
                                         TTF_SCORE = sum(TTF_SCORE * OPPONENT_CASE_PROB) / sum(OPPONENT_CASE_PROB),
                                       TURNS_TO_FINISH = sum(TURNS_TO_FINISH * OPPONENT_CASE_PROB) / sum(OPPONENT_CASE_PROB),
                                      #   CYC_DIST_SCORE = mean(CYC_DIST_SCORE),
                                      MOVE_ORDER_SCORE = sum(MOVE_ORDER_SCORE * OPPONENT_CASE_PROB) / sum(OPPONENT_CASE_PROB),
                                      OVER_FINISH_SCORE = sum(OVER_FINISH_SCORE * OPPONENT_CASE_PROB) / sum(OPPONENT_CASE_PROB)
          ), by = .(CYCLERS, MOVES, TEAM_ID)][order(-EV)]
          print(zoom(game_status))
          print(deck_status[CYCLER_ID %in% smart_cycler_ids  & Zone == "Hand", .(CARDS = paste0(sort(unique(MOVEMENT)), collapse = "-")), by = .(CYCLER_ID)][order(CYCLER_ID)])
          scalued <- check_res2[ ,.(MOVES, EV = round(EV - max(EV), 2),
                                   MOVE_DIFF_SCORE = round(MOVE_DIFF_SCORE, 2),
                                   EXHAUST_SCORE = round(EXHAUST_SCORE - max(EXHAUST_SCORE), 2),
                                   SLOTS_PROGRESSED_SCORE = round(SLOTS_PROGRESSED_SCORE - max(SLOTS_PROGRESSED_SCORE), 2),
                                   TTF_SCORE = round(TTF_SCORE, 2),
                                   TURNS_TO_FINISH = round(TURNS_TO_FINISH, 2),
                                   MOVE_ORDER_SCORE = round(MOVE_ORDER_SCORE - mean(MOVE_ORDER_SCORE), 2),
                                   OVER_FINISH_SCORE = round(OVER_FINISH_SCORE, 2))]

          print(scalued[1:10][order(-EV)])

          check_res2[, M1 := as.numeric(str_sub(MOVES, 1, 1))]
          check_res2[, M2 := as.numeric(str_sub(MOVES, 3, 3))]

          check_res2[, C1 := as.numeric(word(CYCLERS, 1, 1, sep = fixed("_")))]
          check_res2[, C2 := as.numeric(word(CYCLERS, 2, 2, sep = fixed("_")))]


                  check_res2[,  DECK_LEFT_C1 := convert_deck_left_to_text(deck_status, C1), by = .(MOVES )]
                  check_res2[,  DECK_LEFT_C2 := convert_deck_left_to_text(deck_status, C2), by = .(MOVES )]
          check_res2[, draw_odds_C1 := calculate_odds_of_drawing_card(DECK_LEFT_C1, M1)]
          check_res2[, draw_odds_C2 := calculate_odds_of_drawing_card(DECK_LEFT_C2, M2)]


          worse_move1 <- check_res2[, .(MOVEMENT = as.numeric(M1), CYCLER_ID = as.numeric(C1), OTHER_MOVE = as.numeric(M2), OTHER_CYCLER = as.numeric(C2), TEAM_ID, EV, TURNS_TO_FINISH)]
          worse_move2 <- check_res2[, .(MOVEMENT = as.numeric(M2), CYCLER_ID = as.numeric(C2), OTHER_MOVE = as.numeric(M1),  OTHER_CYCLER = as.numeric(C1),TEAM_ID, EV, TURNS_TO_FINISH)]
          append_worse <- rbind(worse_move1, worse_move2)
          sorted_app <- append_worse[order(CYCLER_ID, MOVEMENT, -EV)]
          append_worse[, PRIO := seq_len(.N), by = .(CYCLER_ID, MOVEMENT)]
         # append_worse[CYCLER_ID == 5 & MOVEMENT == 7]
          append_worse[, playing_prob_other := calculate_draw_distribution_by_turn_with_prio(.SD, turn_start_deck), by = .(CYCLER_ID, MOVEMENT), .SDcols = c("OTHER_CYCLER", "OTHER_MOVE", "PRIO")]

          EVs <- append_worse[, .(WEIGHTED_EV = sum(EV * playing_prob_other), TURNS_TO_FINISH = sum(TURNS_TO_FINISH *playing_prob_other)), by = .(MOVEMENT, CYCLER_ID)]
          #copy columns to fit to function
          setorder(EVs, CYCLER_ID, -WEIGHTED_EV)
          EVs[, ':=' (PRIO = seq_len(.N), OTHER_CYCLER = CYCLER_ID, OTHER_MOVE = MOVEMENT), by = CYCLER_ID]
          EVs[, playing_prob_me := calculate_draw_distribution_by_turn_with_prio(.SD, turn_start_deck), by = .(CYCLER_ID), .SDcols = c("OTHER_CYCLER", "OTHER_MOVE", "PRIO")]
          EV_OF_PLAYING_FIRST <- EVs[, sum(WEIGHTED_EV * playing_prob_me), by = CYCLER_ID]


          first_selected_cycler <- EV_OF_PLAYING_FIRST[which.max(V1), CYCLER_ID]
      #    p1_data <- check_res2[, ]



          setkey(MIXED_STRATEGY, PROB_PRODUCT)
          ress <- MIXED_STRATEGY[, tail(.SD, 3), by = TEAM_ID]
          ress[, prio := seq_len(.N), by = TEAM_ID]
          ress[order(TEAM_ID, -prio )]
          #CHOOSE WHICH DECISION MAKE FIRST. The one with bettter winning chance goes last

          #moving_cycler <- choose_first_AI_cycler(smart_team, game_status, "leading", STG_CYCLER)
          #rouler first
          #  moving_cycler <- ADM_CYCLER_INFO[CYCLER_ID %in% smart_cycler_ids & CYCLER_ID %in% in_game_cyclers, CYCLER_ID]
          moving_cycler_dt <- ADM_CYCLER_INFO[TEAM_ID == smart_team & CYCLER_ID %in% in_game_cyclers & CYCLER_ID %in% first_selected_cycler, .(CYCLER_ID = min(CYCLER_ID))]
          #simulate and score

          if (nrow(moving_cycler_dt) > 0) {
            moving_cycler <- moving_cycler_dt[, CYCLER_ID]

            # card_id_selected  <-  card_selector_by_stat(game_status, deck_status[CYCLER_ID == moving_cycler & Zone == "Hand"], moving_cycler, "SMART_MAX",  aim_downhill = TRUE)[, CARD_ID]
            options <- deck_status[CYCLER_ID == moving_cycler & Zone == "Hand", .N, by = .(MOVEMENT)][, MOVEMENT]
            cards_in_hand <- EVs[CYCLER_ID == moving_cycler & MOVEMENT %in% options]
         #   print(cards_in_hand)
            # cards_in_hand <- check_res2[C1 == moving_cycler & M1 %in% options,
            #                                 .N, by = .(M1, MOVES, EV, draw_odds_C2,
            #                                            MOVE_DIFF_SCORE,
            #                                            EXHAUST_SCORE,
            #                                            TTF_SCORE,
            #
            #                                         #   CYC_DIST_SCORE,
            #                                            MOVE_ORDER_SCORE,
            #                                            OVER_FINISH_SCORE)][, N := NULL]


            first_movement <- cards_in_hand[, MOVEMENT[which.max(WEIGHTED_EV)]]
            #convert exhaust if possible
            first_move <- deck_status[CYCLER_ID == moving_cycler & Zone == "Hand" & MOVEMENT == first_movement, min(CARD_ID)]

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
            cards_in_hand <- append_worse[CYCLER_ID == second_cycler & OTHER_MOVE == first_movement & MOVEMENT %in% options]




          #  print(cards_in_hand)
            second_movement <-  cards_in_hand[, MOVEMENT [which.max(EV)]]
            second_move <- deck_status[CYCLER_ID == second_cycler & Zone == "Hand" & MOVEMENT == second_movement, min(CARD_ID)]
            if (second_cycler == 6 ) {
              print(paste0("rouler played: ", first_move))
            print(paste0("sprinteur played: ", second_move))
            } else {
              print(paste0("rouler played: ", second_move))
              print(paste0("sprinteur played: ", first_move))
            }

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
    print(total_winner[TURN_ID == 13])
    #full_action[, .(SLIP = mean(SLIP), BLOCK = mean(BLOCK), EXHAUST = mean(EXHAUST), ASCEND = mean(ASCEND)), by = CYCLER_ID][order(CYCLER_ID)]
  #  print(full_action[, .(
   #                       SLIP = mean(SLIP_BONUS, na.rm = TRUE), BLOCK = mean(BLOCK_DIFF, na.rm = TRUE), EXHAUST = mean(EXHAUST, na.rm = TRUE),
  #                        ASCEND = mean(ASCEND_GAIN, na.rm = TRUE)), by = CYCLER_ID][order(CYCLER_ID)])

    turn_id <- 0
  }
#print(total_winner[, .N, by = .(CYCLER_ID, POSITION)])[order(CYCLER_ID, POSITION)]

