#tab_bot_management

#check if bots needs to move

#hi bot y, what would you do if combinations_data is this. you are team 1.

observeEvent(srv$turn_id, {
 #TASKS

  required_data(c("STG_TRACK_PIECE", "STG_TRACK", "STG_CYCLER", "ADM_CYCLER_INFO"))
  # do we have bots
req(deck_status_curr_game())
req(game_status())
req(input$join_tournament)

  deck_status_now <- deck_status_curr_game()[TURN_ID == srv$turn_id & HAND_OPTIONS == 1]
  prev_turn_in_deck <- deck_status_curr_game()[TURN_ID < srv$turn_id, max(TURN_ID)]
  previous_deck <- deck_status_curr_game()[TURN_ID == prev_turn_in_deck & HAND_OPTIONS == 0]



  any_bots <- tournament()[PLAYER_TYPE == "AI" & TOURNAMENT_NM == input$join_tournament]
bots_teams <- any_bots[, TEAM_ID]
  #unplayed_cards_at_all?

#montako movia pitäisi pelata tällä vuorolla?
all_bot_cyclers   <- ADM_CYCLER_INFO[TEAM_ID %in% bots_teams, CYCLER_ID]
how_manyneeded_total <-  srv$gs_simple[CYCLER_ID %in% all_bot_cyclers, .N]
#montako pelattu?
mf_local_all_turns <- move_fact_data()[TOURNAMENT_NM == input$join_tournament]
mf_local <-  mf_local_all_turns[TURN_ID ==   srv$turn_id & TEAM_ID %in% bots_teams]
how_many_played <- mf_local[CARD_ID > -1   , .N]
how_many_more_needed <- how_manyneeded_total - how_many_played



  if (how_many_more_needed > 0) {
    #we have cards, calculate common data

    tracki <- tournament_data_reactive()[GAME_ID == srv$game_id, .N, by = TRACK_ID][, TRACK_ID]

    pre_aggr_game_status <- precalc_track(game_status())
    pre_agg_no_list <- pre_aggr_game_status$aggr_to_slots
    ijk <- ijk_map(tracki, STG_TRACK_PIECE, STG_TRACK)

    matr_ijk <- as.matrix(ijk)
    slip_map <- slipstream_map(tracki, STG_TRACK_PIECE, STG_TRACK)
    slip_map_matrix <- as.matrix(slip_map)

    slots_squares <- as.matrix(game_status()[, .(SQUARE_ID, GAME_SLOT_ID)])
    reverse_slots_squares <- slots_squares[nrow(slots_squares):1,]

    calc_ttf_input_all <- ifelse(turn_id >= 5, 0, turn_id)
    combinations_output <- calc_combinations_data(con, game_status(), previous_deck,
                                                  pre_agg_no_list, matr_ijk, reverse_slots_squares, slip_map_matrix, STG_CYCLER, calc_ttf = calc_ttf_input_all)

    MIXED_STRATEGY <- calculate_mixed_strategy(combinations_output, consensus_config_id = NA,  previous_deck)


  for (bot_loop in any_bots[, TEAM_ID]) {


#do I need to play?
    my_cyclers  <- ADM_CYCLER_INFO[TEAM_ID == bot_loop, CYCLER_ID]
    #montako pelattu?
    mf_local_loop <-  mf_local_all_turns[TURN_ID ==   srv$turn_id & TEAM_ID %in% bot_loop]
    how_many_played_loop <- mf_local_loop[CARD_ID > -1   , .N]
    how_manyneeded_total_loop <-  srv$gs_simple[CYCLER_ID %in% my_cyclers, .N]
    how_many_more_needed_loop <- how_manyneeded_total_loop - how_many_played_loop
    if (how_many_more_needed_loop > 0) {






      #for each bot

  hidden_information_output <- update_combinations_with_hidden_input(MIXED_STRATEGY$combinations,
                                                                     deck_status_now, team_id_input = bot_loop, pre_agg_no_list, calc_ttf =  srv$turn_id)



  bot_config <- NA
  funcargs <- list(hidden_information_output, deck_status_now,
                   bot_config, bot_loop, pre_agg_no_list)
  myfunc <- "ruler_bot"
  # debug <- finish_rank_bot(hidden_information_output, deck_status_now,
  #                          bot_config, bot_loop, pre_agg_no_list)

  res <- do.call(myfunc, funcargs)
  print(res)

  moves <- EV_to_moves(res, deck_status_now)
  print(moves)
  first_cyc <-  moves[FIRST == 1, CYCLER_ID]
  first_move <- moves[FIRST == 1, CARD_ID]
  second_cycler <- moves[FIRST == 0, CYCLER_ID]
  second_move <-  moves[FIRST == 0, CARD_ID]
  cyclers_left <- game_status()[CYCLER_ID > 0, CYCLER_ID]


  my_options <- ADM_CYCLER_INFO[CYCLER_ID %in% cyclers_left & TEAM_ID == bot_loop, .N]



  #write to db update UI
  con <- connDB(con, "flaimme")

  turni <- srv$turn_id
  game <- srv$game_id

  write_data_first <- data.table(TOURNAMENT_NM = input$join_tournament,
                                 FIRST_SELECTED = 1,
                                 CYCLER_ID = first_cyc,
                                 CARD_ID = first_move,
                                 GAME_ID = game,
                                 TURN_ID = turni,
                                 TEAM_ID = bot_loop)


  if (my_options == 2) {
    write_data_second <- data.table(TOURNAMENT_NM = input$join_tournament,
                                    FIRST_SELECTED = 0,
                                    CYCLER_ID = second_cycler,
                                    CARD_ID = second_move,
                                    TURN_ID = turni,
                                    GAME_ID = game,
                                    TEAM_ID = bot_loop )
  } else {
    write_data_second <- NULL
  }


  appendaa <- rbind(write_data_first, write_data_second)


  con <- connDB(con, "flaimme")

  dbWriteTable(con, "MOVE_FACT", appendaa, row.names = FALSE, append = TRUE)
  #move_fact$data <- dbSelectAll("MOVE_FACT", con)[GAME_ID == srv$game_id & TOURNAMENT_NM == input$join_tournament]
  #wirte to MOVE_FACT a row per cycler
  #update MOVE_FACT with selections

  }
  }
  }

  })
observe({

  req( input$join_tournament)
  req( srv$game_id)
  ba_card_data <- breakaway_bet_cards_data()[TOURNAMENT_NM ==  input$join_tournament]


  any_bots <- tournament()[PLAYER_TYPE == "AI" & TOURNAMENT_NM == input$join_tournament]
  bots_teams <- any_bots[, TEAM_ID]

  if (length(bots_teams) > 0) {
  #if we have rows here, we are middle of ba bet
  if (nrow(ba_card_data) > 0) {

    ba_data <-  breakaway_bet_data()[GAME_ID == srv$game_id & TOURNAMENT_NM ==  input$join_tournament]
    for (bot_loop in any_bots[, TEAM_ID]) {
    # do I need to bet?

      my_cyclers  <- ADM_CYCLER_INFO[TEAM_ID == bot_loop, CYCLER_ID]
      #have I selected a cycler?
      selected_bot <- ba_data[CYCLER_ID %in% my_cyclers]
      #if I have one row, then I have. If 0, then I need to select
      gs <- game_status_simple()[GAME_ID == srv$game_id & TURN_ID == -1 & CYCLER_ID %in% my_cyclers]
      if (nrow(selected_bot) == 0) {
        #now selected cycler. Take the one of has worst grid position

        worst_cycler <- gs[which.min(SQUARE_ID), CYCLER_ID]
        ins_row <- data.table(TOURNAMENT_NM = input$join_tournament,
                              TEAM_ID = bot_loop,
                              CYCLER_ID = worst_cycler,
                              GAME_ID = srv$game_id,
                              FIRST_BET = 0,
                              SECOND_BET = 0)


        con <- connDB(con, "flaimme")
        dbIns("BREAKAWAY_BET",
              ins_row,
              con)


        # I can straight away continue by selecting the card
        #I have not selected first card. Do it now
        #I would end up in slot 10. what is my current slot?
        me_cycler <- worst_cycler
        curr_slot <- ceiling(gs[CYCLER_ID == me_cycler, SQUARE_ID] / 2)
        #aim to bet difference + 1. Read meta too

        meta_data <-  breakaway_bet_data()[GAME_ID != srv$game_id & TOURNAMENT_NM ==  input$join_tournament]
        meta_data[, TOT_BET := FIRST_BET +  SECOND_BET]
        setorder(meta_data, TOT_BET)
        meta_second <- meta_data[, .SD[2], GAME_ID]
        mean_2nd_winner <- meta_second[, mean(TOT_BET)]

        target <- 10 - curr_slot +  1
        if (is.nan(mean_2nd_winner)) {
          mean_2nd_winner <- target
        }
        #offer based on meta, but within +-1 of target
        meta_target <- max(min(mean_2nd_winner, target + 1), target - 1)
        options <- ba_card_data[CYCLER_ID == me_cycler & HAND_NUMBER == 1]
        target_first_card <- meta_target / 2
        options[, diff_to_target := abs(target_first_card - CARD_ID + 0.1)]
        #read meta
        selected_card <- options[which.min(diff_to_target), .(CARD_ID)][, .N, by = CARD_ID][, CARD_ID]


        dbQ(paste0('UPDATE BREAKAWAY_BET
             SET FIRST_BET = ', selected_card,
                   ' WHERE GAME_ID = ', srv$game_id, ' AND TEAM_ID = ', bot_loop, ' AND TOURNAMENT_NM = "', input$join_tournament, '"'), con)

      }

      if (nrow(selected_bot) == 1) {
        #have I selected first card?
        if (selected_bot[, FIRST_BET] > 0) {
          #yes I have. Have everyone else?
          #have everyone selected their bettor?
          count_bettors <- ba_data[, .N]
          count_all_cyclers <- game_status_simple()[GAME_ID == srv$game_id & TURN_ID == -1, .N]
          #one cycler per team
          if (count_bettors == count_all_cyclers / 2) {
            #everyone have selected. Now check if everyone have selceted first bet
            count_missing_bet <- ba_data[FIRST_BET == 0, .N]
            if (count_missing_bet == 0) {

              #all have bet first, we can bet second
              me_cycler <- selected_bot[, CYCLER_ID]
              curr_slot <- ceiling(gs[CYCLER_ID == me_cycler, SQUARE_ID] / 2)
              my_first_bet <- selected_bot[, FIRST_BET]
              #aim to bet difference + 1. Read meta too

              meta_data <-  breakaway_bet_data()[GAME_ID != srv$game_id & TOURNAMENT_NM ==  input$join_tournament]
              meta_data[, TOT_BET := FIRST_BET +  SECOND_BET]
              setorder(meta_data, TOT_BET)
              meta_second <- meta_data[, .SD[2], GAME_ID]
              mean_2nd_winner <- meta_second[, mean(TOT_BET)]

              target <- 10 - curr_slot +  1
              if (is.nan(mean_2nd_winner)) {
                mean_2nd_winner <- target
              }
              #offer based on meta, but within +-1 of target
              meta_target <- max(min(mean_2nd_winner, target + 1), target - 1)
              options <- ba_card_data[CYCLER_ID == me_cycler & HAND_NUMBER == 1]
              target_second_card <- meta_target - my_first_bet
              options[, diff_to_target := abs(target_second_card - CARD_ID + 0.1)]
              #read meta
              selected_card <- options[which.min(diff_to_target), .(CARD_ID)][, .N, by = CARD_ID][, CARD_ID]
              con <- connDB(con, "flaimme")

              dbQ(paste0('UPDATE BREAKAWAY_BET
             SET SECOND_BET = ', selected_card,
                         ' WHERE GAME_ID = ', srv$game_id, ' AND TEAM_ID = ', bot_loop, ' AND TOURNAMENT_NM = "', input$join_tournament, '"'), con)

            }
          }
        }

      }

    }
  }
  }








  # what cards I have?
  #what is my start position?
  #which cycler I bet?
  #which card bet?
  #read what others bet
  #choose other card




})

#read cards and gamestate

#send info to correct bot

#wait bot to choose

#save result
