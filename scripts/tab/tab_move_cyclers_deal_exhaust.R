#tab_move_cyclers_deal_exhaust

observe({

  #UPDATE GAME STATE IF MOVES ARE PLAYED. OBSERVE MOVE FACT. ALSO UPDATE turn
  req(input$join_tournament)
  #if CARD_ID > 0 for each cycler id where turn_id > max(turn_id) from GAME_STATUS
  #find max turn from game_status

  if (nrow(srv$gs_simple) > 0 & nrow(move_fact_data()) > 0) {

  game <- srv$gs_simple[, max(GAME_ID)]

  mf_local_all_turns <- move_fact_data()[TOURNAMENT_NM == input$join_tournament]

  #what turn is it?

  #srv$turn_id <- mf_local_all_turns[GAME_ID == game, max(TURN_ID)]
  srv$turn_id <- deck_status_curr_game()[, max(TURN_ID)]
  previous_finished_turn <- srv$gs_simple[GAME_ID == game, max(TURN_ID)]
  mf_local <-  mf_local_all_turns[TURN_ID ==   srv$turn_id]


  how_manyneeded_total <-  srv$gs_simple[, .N]
  how_many_played <- mf_local[CARD_ID > -1   , .N]
  how_many_more_needed <- how_manyneeded_total - how_many_played

  #and check that we have started a new turn
  new_turn_check <- previous_finished_turn != srv$turn_id

  if (how_many_more_needed == 0 & new_turn_check) {

    con <- connDB(con, "flaimme")
    deck_status <- dbQ(paste0('SELECT * FROM DECK_STATUS WHERE TURN_ID = ', srv$turn_id, ' AND GAME_ID = ',game,
                              ' AND TOURNAMENT_NM = "', input$join_tournament, '"'), con)

    srv$game_status <- create_game_status_from_simple(srv$gs_simple, srv$track_id_input, STG_TRACK, STG_TRACK_PIECE)
    #all played, then make the moves and play the cards
    action_data <- mf_local
    #move order
    #game state is behind one turn

    move_order_cyclers <- srv$gs_simple[order(-SQUARE_ID), CYCLER_ID]

    for (loop_cycler in move_order_cyclers) {
      card_id_played <- action_data[CYCLER_ID == loop_cycler, CARD_ID]
      movement <- ifelse(card_id_played == 1, 2, card_id_played)
      srv$game_status <- move_cycler(current_game_status_input = srv$game_status, cycler_id = loop_cycler, movement = movement)

      deck_status <- play_card(loop_cycler, card_id = card_id_played, deck_status, game_id = -100, turn_id = -100, con = NULL)
    }
    #slipstream and exhaustion
    srv$game_status <- apply_slipstream(srv$game_status)


    #check winners
    #RECROD WINNER
    finish_lane <- srv$game_status[FINISH == 1, max(GAME_SLOT_ID)]
    finish_slots <-  srv$game_status[GAME_SLOT_ID >= finish_lane]
    #any there?

    finishers <- finish_slots[order(-SQUARE_ID)][CYCLER_ID > 0]
    if (nrow(finishers) > 0) {

      finished <- finishers[, CYCLER_ID]
      for (finish_loop in finished) {
        #check which game_id we are playing,
        srv$game_id <- mf_local_all_turns[, max(GAME_ID)]
        sof <- srv$game_status[CYCLER_ID == finish_loop, GAME_SLOT_ID] - finish_lane + 1
        finish_turn <- srv$turn_id
        lane <- srv$game_status[CYCLER_ID == finish_loop, LANE_NO]
        ex_left <- deck_status[CYCLER_ID == finish_loop & CARD_ID == 1 & TURN_ID == srv$turn_id, .N]
        dbQ(paste0('UPDATE TOURNAMENT_RESULT SET FINISH_TURN = ', finish_turn,
                   ',SLOTS_OVER_FINISH = ', sof ,
                   ', LANE = ', lane ,
                   ', EXHAUST_LEFT = ',ex_left ,
                   ' WHERE CYCLER_ID = ',finish_loop,
                   ' AND TOURNAMENT_NM = "', input$join_tournament, '" AND GAME_ID = ', srv$game_id ), con)
      }
      srv$game_status <- clear_finishers(srv$game_status, finished)
    }



    # we apply exhaustion to finishers but it does not matter it has been saved already to db
    deck_status <- apply_exhaustion(deck_status, srv$game_status)

    #now we can clear finishers

    simple_gs <- srv$game_status[CYCLER_ID > 0, .(CYCLER_ID, SQUARE_ID)]
    simple_gs[, TOURNAMENT_NM := input$join_tournament]
    simple_gs[, GAME_ID := srv$game_id]
    simple_gs[, TURN_ID := srv$turn_id]
    srv$gs_simple <- simple_gs

    deck_status[, TOURNAMENT_NM := input$join_tournament]
    deck_status[, GAME_ID := srv$game_id]
    deck_status[, HAND_OPTIONS := 0]
    deck_status[, TURN_ID := srv$turn_id]
    dbWriteTable(con, "DECK_STATUS", deck_status, append = TRUE, row.names = FALSE)

    dbWriteTable(con, "GAME_STATUS", simple_gs, row.names = FALSE, append = TRUE)


    #deal new cards
    cyclers_left <- simple_gs[CYCLER_ID > 0, CYCLER_ID]

    if (length(cyclers_left) > 0) {
    for (cycler_loop in cyclers_left) {
      deck_status <- draw_cards(cycler_loop, deck_status, 4)
    }
    deck_status[, HAND_OPTIONS := 1]
    deck_status[, TURN_ID :=  srv$turn_id + 1]
    dbWriteTable(con, "DECK_STATUS", deck_status, append = TRUE, row.names = FALSE)
    }


  }
  }
})
