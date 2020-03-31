#tab_game_start


observe({

  req(input$join_tournament)
  #launch when gets a command to start
  command_rows <- command_data()[TOURNAMENT_NM == input$join_tournament, .(COMMAND, COMMAND_ID)]
  first_command <- command_rows[1, COMMAND]
  if (is.na(first_command)) {
    first_command <- ""
  }
  if (first_command == "SETUP") {
    print("running SETUP command")
  #CREATES DECKS AND GAME_STATE AT STARTUP.
  #we have also new game_id here
  req(tournament_data_reactive())
  new_game_found <- tournament_data_reactive()
  max_row <- new_game_found[, .N]
  tour_name <- new_game_found[max_row, TOURNAMENT_NM]
  srv$game_id <- new_game_found[max_row, GAME_ID]
  srv$track_id_input <-new_game_found[max_row, TRACK_ID]
  #check if it is already running
  con <- connDB(con, "flaimme")

  #check taht we have at least one tournament
  if (nrow(new_game_found) > 0) {
    rows_from_the_game <- dbQ(paste0('SELECT * FROM DECK_STATUS WHERE TOURNAMENT_NM = "', tour_name, '" AND GAME_ID = "',
                                     srv$game_id, '"'),con)
    game_status_done <- dbQ(paste0('SELECT * FROM GAME_STATUS WHERE TOURNAMENT_NM = "', tour_name, '" AND GAME_ID = "',
                                   srv$game_id, '"'),con)


    if (nrow(rows_from_the_game) > 0 & nrow(game_status_done) > 0) {
      #RUNNING

    } else {

      #not running. start the game
      #just in case delete partial rows if somehoe only deck_status or game_status was setup
      dbQ(paste0('DELETE FROM DECK_STATUS WHERE TOURNAMENT_NM = "', tour_name, '" AND GAME_ID = "',
                 srv$game_id, '"'), con )
      dbQ(paste0('DELETE FROM GAME_STATUS WHERE TOURNAMENT_NM = "', tour_name, '" AND GAME_ID = "',
                 srv$game_id, '"'), con )


      cycler_ids <- new_game_found[GAME_ID == srv$game_id & TOURNAMENT_NM == tour_name][order(START_POSITION), CYCLER_ID]
      #create decks

      deck_status <- create_decks(cycler_ids, ADM_CYCLER_DECK)

      #add exhaust

      exh_add_amount <- new_game_found[GAME_ID == (srv$game_id - 1) & TOURNAMENT_NM == tour_name & EXHAUST_LEFT > 0, .(CYCLER_ID, EXHAUST_LEFT)]
      if (nrow(exh_add_amount) > 0) {
        deck_status <- add_startup_exhaustion(exh_add_amount, deck_status)

        #update to db too
        for (ex_loop in exh_add_amount[, CYCLER_ID]) {
          dbQ(paste0('UPDATE TOURNAMENT_RESULT SET STARTING_EXHAUST = ', exh_add_amount[CYCLER_ID == ex_loop, EXHAUST_LEFT],
                     ' WHERE CYCLER_ID = ', ex_loop), con)
        }
      }



      #write decks to db
      info_joined <- copy(deck_status)
      info_joined[, ':=' (GAME_ID = srv$game_id,
                          TOURNAMENT_NM = tour_name,
                          TURN_ID = -1,
                          HAND_OPTIONS = 0)]
      dbWriteTable(con, "DECK_STATUS", info_joined, append = TRUE, row.names = FALSE)

      #setup initial game status

      track <- new_game_found[GAME_ID == srv$game_id  & TOURNAMENT_NM == tour_name, mean(TRACK_ID)]
      srv$game_status <- start_game(cycler_ids ,track, STG_TRACK_PIECE, STG_TRACK)
      simple_gs <- srv$game_status[CYCLER_ID > 0, .(CYCLER_ID, SQUARE_ID)]
      simple_gs[, TOURNAMENT_NM := tour_name]
      simple_gs[, GAME_ID := srv$game_id]
      simple_gs[, TURN_ID := -1]
      srv$gs_simple <- simple_gs

      dbWriteTable(con, "GAME_STATUS", simple_gs, row.names = FALSE, append = TRUE)

    }
  } else {
    returnV <- NULL
    returnV
  }
  rel_com_id <- command_rows[1, COMMAND_ID]
  dbQ(paste0('DELETE FROM CLIENT_COMMANDS WHERE COMMAND_ID = ', rel_com_id), con)
  }
})



observe({

  req(input$join_tournament)
  #launch when gets a command to s
  command_rows <- command_data()[TOURNAMENT_NM == input$join_tournament, .(COMMAND, COMMAND_ID)]
  first_command <- command_rows[1, COMMAND]
  if (is.na(first_command)) {
    first_command <- ""
  }
  if (first_command == "UPDATE_TRACKS") {
    required_data("SRC_TRACK", force_update = TRUE)
    required_data("STG_TRACK", force_update = TRUE)
    rel_com_id <- command_rows[1, COMMAND_ID]
    dbQ(paste0('DELETE FROM CLIENT_COMMANDS WHERE COMMAND_ID = ', rel_com_id), con)
  }

})



observe({

  req(input$join_tournament)
  command_rows <- command_data()[TOURNAMENT_NM == input$join_tournament, .(COMMAND, COMMAND_ID)]
  first_command <- command_rows[1, COMMAND]
  if (is.na(first_command)) {
    first_command <- ""
  }
  if (first_command == "BREAKAWAY") {
  #break away options
    print("running BREAKAWAY command")

  #write options to db
  hand_numbers <- c(1, 2)
  #delete previous

  dbQ(paste0('DELETE FROM BREAKAWAY_BET_CARDS WHERE TOURNAMENT_NM = "', input$join_tournament, '"'), con)
  new_game_found <- tournament_data_reactive()
  max_row <- new_game_found[, .N]
  tour_name <- new_game_found[max_row, TOURNAMENT_NM]
  cycler_ids <- new_game_found[GAME_ID == srv$game_id & TOURNAMENT_NM == tour_name][order(START_POSITION), CYCLER_ID]

  deck_status <- dbQ(paste0('SELECT * FROM DECK_STATUS WHERE TURN_ID = -1 AND GAME_ID = ', srv$game_id,
                            ' AND TOURNAMENT_NM = "', input$join_tournament, '"'), con)
  for (hand_loop in hand_numbers) {
    for (cycler_loop in cycler_ids) {
      deck_status <- draw_cards(cycler_loop, deck_status, 4)
    }
    only_hand_cards <- deck_status[Zone == "Hand", .(CYCLER_ID, CARD_ID,
                                                     TOURNAMENT_NM = input$join_tournament,
                                                     GAME_ID = srv$game_id,
                                                     HAND_NUMBER = hand_loop)]

    dbWriteTable(con, "BREAKAWAY_BET_CARDS", only_hand_cards, row.names = FALSE, append = TRUE)
    deck_status[Zone == "Hand", Zone := "Recycle"]
  }

  rel_com_id <- command_rows[1, COMMAND_ID]
  dbQ(paste0('DELETE FROM CLIENT_COMMANDS WHERE COMMAND_ID = ', rel_com_id), con)

}
})



observe({

  req(input$join_tournament)
  command_rows <- command_data()[TOURNAMENT_NM == input$join_tournament, .(COMMAND, COMMAND_ID)]
  first_command <- command_rows[1, COMMAND]
  if (is.na(first_command)) {
    first_command <- ""
  }
  if (first_command == "START") {
print("running start command")
  #my job is to monitor commands and decide to deal first cards of the game
req(input$join_tournament)

  tournament_result <- tournament_data_reactive()
  joinai <- tournament_result[TOURNAMENT_NM == input$join_tournament & LANE == -1, .N, by = .(GAME_ID, TOURNAMENT_NM)]
  if (nrow(joinai) > 0) {






  #clear breakaway bet cards
  con <- connDB(con, "flaimme")
  dbQ(paste0('DELETE FROM BREAKAWAY_BET_CARDS WHERE TOURNAMENT_NM = "', input$join_tournament, '"'), con)



    joinai <- tournament_result[TOURNAMENT_NM == input$join_tournament & LANE == -1, .N, by = .(GAME_ID, TOURNAMENT_NM)]



     start_loop <- 1 #here was previously loop

        deck_status_all <- dbQ(paste0('SELECT * FROM DECK_STATUS WHERE GAME_ID = ', joinai[start_loop, GAME_ID],
                                  ' AND TOURNAMENT_NM = "', joinai[start_loop, TOURNAMENT_NM], '"'), con)
        max_deck_turn <- deck_status_all[, max(TURN_ID)]
        #check if we had breakaway bets. If not, update deck status and game status turn to 0

        if (max_deck_turn == -1) {
          dbQ(paste0('UPDATE DECK_STATUS SET TURN_ID = ', 0, ' WHERE GAME_ID = ', joinai[start_loop, GAME_ID],
                     ' AND TOURNAMENT_NM = "', joinai[start_loop, TOURNAMENT_NM], '"'), con)
          dbQ(paste0('UPDATE GAME_STATUS SET TURN_ID = ', 0, ' WHERE GAME_ID = ', joinai[start_loop, GAME_ID],
                     ' AND TOURNAMENT_NM = "', joinai[start_loop, TOURNAMENT_NM], '"'), con)
        }


        deck_status <- deck_status_all[TURN_ID == max_deck_turn]

        #draw for each
        cyclers <- tournament_result[TOURNAMENT_NM == joinai[start_loop, TOURNAMENT_NM] & GAME_ID == joinai[start_loop, GAME_ID], CYCLER_ID]

        for (cycler_loop in cyclers) {
          deck_status <- draw_cards(cycler_loop, deck_status, 4)
          if(deck_status[CYCLER_ID == cycler_loop & Zone == "Hand", .N ] != 4) {
            browser()
          }
        }

        deck_status[, TOURNAMENT_NM := joinai[start_loop, TOURNAMENT_NM]]
        deck_status[, GAME_ID := joinai[start_loop, GAME_ID]]
        deck_status[, HAND_OPTIONS := 1]
        deck_status[, TURN_ID := 1]
        dbWriteTable(con, "DECK_STATUS", deck_status, append = TRUE, row.names = FALSE)
        srv$turn_id <- 1
    }

  rel_com_id <- command_rows[1, COMMAND_ID]
  dbQ(paste0('DELETE FROM CLIENT_COMMANDS WHERE COMMAND_ID = ', rel_com_id), con)
  }
})
