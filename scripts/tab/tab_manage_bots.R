#tab_bot_management

#check if bots needs to move

#hi bot y, what would you do if combinations_data is this. you are team 1.

observeEvent(srv$turn_id, {
 #TASKS
  required_data(c("STG_TRACK_PIECE", "STG_TRACK", "STG_CYCLER", "ADM_CYCLER_INFO"))
  # do we have bots
req(deck_status_curr_game())
req(game_status())

  deck_status_now <- deck_status_curr_game()[TURN_ID == srv$turn_id & HAND_OPTIONS == 1]
  prev_turn_in_deck <- deck_status_curr_game()[TURN_ID < srv$turn_id, max(TURN_ID)]
  previous_deck <- deck_status_curr_game()[TURN_ID == prev_turn_in_deck & HAND_OPTIONS == 0]



  any_bots <- tournament()[PLAYER_TYPE == "AI" & TOURNAMENT_NM == input$join_tournament]

  for (bot_loop in any_bots[, TEAM_ID]) {

#do I need to play?
    my_cyclers  <- ADM_CYCLER_INFO[TEAM_ID == bot_loop, CYCLER_ID]
    unplayed_cards <- deck_status_now[CYCLER_ID  %in% my_cyclers & Zone == "Hand"][, .N]
    if (unplayed_cards > 0) {

  tracki <- tournament_data_reactive()[GAME_ID == srv$game_id, .N, by = TRACK_ID][, TRACK_ID]





  pre_aggr_game_status <- precalc_track(game_status())
  pre_agg_no_list <- pre_aggr_game_status$aggr_to_slots
  ijk <- ijk_map(tracki, STG_TRACK_PIECE, STG_TRACK)

  matr_ijk <- as.matrix(ijk)
  slip_map <- slipstream_map(tracki, STG_TRACK_PIECE, STG_TRACK)
  slip_map_matrix <- as.matrix(slip_map)

  slots_squares <- as.matrix(game_status()[, .(SQUARE_ID, GAME_SLOT_ID)])
  reverse_slots_squares <- slots_squares[nrow(slots_squares):1,]
  rm("ADM_OPTIMAL_MOVES", envir = globalenv())
  required_data("ADM_OPTIMAL_MOVES")

  combinations_output <- calc_combinations_data(con, game_status(), previous_deck,
                                                pre_agg_no_list, matr_ijk, reverse_slots_squares, slip_map_matrix, STG_CYCLER, calc_ttf = TRUE)

  MIXED_STRATEGY <- calculate_mixed_strategy(combinations_output, consensus_config_id = NA,  previous_deck)

  #for each bot
  rm("ADM_OPTIMAL_MOVES", envir = globalenv())
  required_data("ADM_OPTIMAL_MOVES")

  hidden_information_output <- update_combinations_with_hidden_input(MIXED_STRATEGY$combinations,
                                                                     deck_status_now, team_id_input = bot_loop, pre_agg_no_list)



  bot_config <- NA
  funcargs <- list(hidden_information_output, deck_status_now,
                   bot_config, bot_loop, pre_agg_no_list)
  myfunc <- "finish_rank_bot"
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
  })
#read cards and gamestate

#send info to correct bot

#wait bot to choose

#save result
