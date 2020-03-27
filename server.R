
luettu <- dbSelectAll("ADM_OPTIMAL_MOVES", con)

ADM_OPTIMAL_MOVES <- fix_colnames(luettu)



shinyServer(function(input, output, session) {

  con <- connDB(con, "flaimme")
  srv <- reactiveValues(game_status = NULL,
                        gs_simple = dbSelectAll("GAME_STATUS", con))

#
#   #monitor user inputs
#   read_max_game_id <- function() {
#
#     con <- connDB(con, "flaimme")
#     dbQ("SELECT max(GAME_ID) from TOURNAMENT_RESULT", con)
#   }
#
#   read_tournament_result <- function() {
#     con <- connDB(con, "flaimme")
#     dbSelectAll("TOURNAMENT_RESULT", con)
#   }


  #tour_info_data <-  my_reactivePoll(session, "TOURNAMENT_RESULT", "SELECT * FROM TOURNAMENT", 1000, con)
 tournament_data_reactive <- my_reactivePoll(session, "TOURNAMENT_RESULT", "SELECT * FROM TOURNAMENT_RESULT", 2000, con)
 tournament <- my_reactivePoll(session, "TOURNAMENT", "SELECT * FROM TOURNAMENT", 2000, con)
 breakaway_bet_data <- my_reactivePoll(session, "BREAKAWAY_BET", "SELECT * FROM BREAKAWAY_BET", 3000, con)

 breakaway_bet_cards_data <- my_reactivePoll(session, "BREAKAWAY_BET_CARDS", "SELECT * FROM BREAKAWAY_BET_CARDS", 2000, con)

 move_fact_data  <- my_reactivePoll(session, "MOVE_FACT", "SELECT sum(CARD_ID) from MOVE_FACT", 1000, con)

observe({

  req(input$join_tournament)
  #if CARD_ID > 0 for each cycler id where turn_id > max(turn_id) from GAME_STATUS
  #find max turn from game_status
  game <- srv$gs_simple[TOURNAMENT_NM == input$join_tournament, max(GAME_ID)]
  gs_turn <- srv$gs_simple[TOURNAMENT_NM == input$join_tournament & GAME_ID == game, max(TURN_ID)]

    how_many_unplayed <- move_fact_data()[TOURNAMENT_NM == input$join_tournament & CARD_ID == -1  & TURN_ID > gs_turn, .N]
  how_many_played <- move_fact_data()[TOURNAMENT_NM == input$join_tournament & CARD_ID > -1  & TURN_ID > gs_turn, .N]

  deck_status <- dbQ(paste0('SELECT * FROM DECK_STATUS WHERE GAME_ID = ',game,
                            ' AND TOURNAMENT_NM = "', input$join_tournament, '"'), con)
  browser()
  if (how_many_unplayed == 0  & how_many_played > 0) {
    #all played, then make the moves and play the cards
    action_data <- move_fact_data()[TOURNAMENT_NM == input$join_tournament & TURN_ID > gs_turn]
    #move order
    move_order_cyclers <- srv$gs_simple[TURN_ID == gs_turn][order(-SQUARE_ID), CYCLER_ID]
    for (loop_cycler in move_order_cyclers) {
      card_id_played <- action_data[CYCLER_ID == loop_cycler, CARD_ID]
      movement <- ifelse(card_id_played == 1, 2, card_id_played)
      srv$game_status <- move_cycler(current_game_status_input = srv$game_status, cycler_id = loop_cycler, movement = movement)
      deck_status <- play_card(loop_cycler, deck_status, game_id = -1, turn_id = -1, con = FALSE, card_id = card_id_played)
    }
   #slipstream and exhaustion
    srv$game_status <- apply_slipstream(srv$game_status)

    deck_status <- apply_exhaustion(deck_status, srv$game_status)

  }
})

 observe({

   #my job is to monitor breakaway_bet_cards and decide to deal first cards of the game
   #rule, no breakaway_bet_cards in my tournament and tournament_result has been initialized
   tournament_result <- tournament_data_reactive()
   max_turn  <-  srv$gs_simple[, .(max_turn = max(TURN_ID)), by = .(TOURNAMENT_NM, GAME_ID)]

   if (nrow(max_turn) > 0) {
     inited_games <- max_turn[max_turn < 1, .(TOURNAMENT_NM, GAME_ID)]


     cards_amount <- breakaway_bet_cards_data()
     games_not_ready_to_start <- cards_amount[, .N, by = .(GAME_ID, TOURNAMENT_NM)]
     #here we get list of games that can be started
     joinai <- games_not_ready_to_start[inited_games, on = .(GAME_ID, TOURNAMENT_NM)][is.na(N)]
    if (nrow(joinai) > 0) {
     for (start_loop in 1:nrow(joinai)) {

       deck_status <- dbQ(paste0('SELECT * FROM DECK_STATUS WHERE GAME_ID = ', joinai[start_loop, GAME_ID],
                  ' AND TOURNAMENT_NM = "', joinai[start_loop, TOURNAMENT_NM], '"'), con)
       #draw for each
       cyclers <- tournament_result[TOURNAMENT_NM == joinai[start_loop, TOURNAMENT_NM], CYCLER_ID]
       for (cycler_loop in cyclers) {
         deck_status <- draw_cards(cycler_loop, deck_status, 4)
       }

       deck_status[, TOURNAMENT_NM := joinai[start_loop, TOURNAMENT_NM]]
       deck_status[, GAME_ID := joinai[start_loop, GAME_ID]]
       deck_status[, HAND_OPTIONS := 1]
       deck_status[, TURN_ID := 1]
       dbWriteTable(con, "DECK_STATUS", deck_status, append = TRUE, row.names = FALSE)

     }


    }
    }
 })

observe({

  req(tournament_data_reactive())
  new_game_found <- tournament_data_reactive()
  max_row <- new_game_found[, .N]
  tour_name <- new_game_found[max_row, TOURNAMENT_NM]
  new_game_id <- new_game_found[max_row, GAME_ID]
  #check if it is already running
  con <- connDB(con, "flaimme")

  #check taht we have at least one tournament
  if (nrow(new_game_found) > 0) {
  rows_from_the_game <- dbQ(paste0('SELECT * FROM DECK_STATUS WHERE TOURNAMENT_NM = "', tour_name, '" AND GAME_ID = "',
                                  new_game_id, '"'),con)
  game_status_done <- dbQ(paste0('SELECT * FROM GAME_STATUS WHERE TOURNAMENT_NM = "', tour_name, '" AND GAME_ID = "',
                                 new_game_id, '"'),con)


    if (nrow(rows_from_the_game) > 0 & nrow(game_status_done) > 0) {
      #RUNNING
    } else {
      #not running. start the game
      #just in case delete partial rows if somehoe only deck_status or game_status was setup
      dbQ(paste0('DELETE FROM DECK_STATUS WHERE TOURNAMENT_NM = "', tour_name, '" AND GAME_ID = "',
                                 new_game_id, '"'), con )
      dbQ(paste0('DELETE FROM GAME_STATUS WHERE TOURNAMENT_NM = "', tour_name, '" AND GAME_ID = "',
                 new_game_id, '"'), con )


      cycler_ids <- new_game_found[GAME_ID == new_game_id & TOURNAMENT_NM == tour_name, CYCLER_ID]
      #create decks
      deck_status <- create_decks(cycler_ids, ADM_CYCLER_DECK)

      #add exhaust
      exh_add_amount <- new_game_found[GAME_ID == (new_game_id - 1) & TOURNAMENT_NM == tour_name & EXHAUST_LEFT > 0, .(CYCLER_ID, EXHAUST_LEFT)]
       if (nrow(exh_add_amount) > 0) {
          deck_status <- add_startup_exhaustion(exh_add_amount, deck_status)
       }

      #write decks to db
      info_joined <- copy(deck_status)
      info_joined[, ':=' (GAME_ID = new_game_id,
                          TOURNAMENT_NM = tour_name,
                          TURN_ID = -1,
                          HAND_OPTIONS = 0)]
      dbWriteTable(con, "DECK_STATUS", info_joined, append = TRUE, row.names = FALSE)

      #break away options



      #write options to db
      hand_numbers <- c(1, 2)
      #delete previous
      dbQ(paste0('DELETE FROM BREAKAWAY_BET_CARDS WHERE TOURNAMENT_NM = "', tour_name, '"'), con)

      for (hand_loop in hand_numbers) {
        for (cycler_loop in cycler_ids) {
          deck_status <- draw_cards(cycler_loop, deck_status, 4)
        }
        only_hand_cards <- deck_status[Zone == "Hand", .(CYCLER_ID, CARD_ID,
                                                         TOURNAMENT_NM = tour_name,
                                                         GAME_ID = new_game_id,
                                                         HAND_NUMBER = hand_loop)]

        dbWriteTable(con, "BREAKAWAY_BET_CARDS", only_hand_cards, row.names = FALSE, append = TRUE)
        deck_status[Zone == "Hand", Zone := "Recycle"]
      }


      #set cards back to hand. "I dont know if this matters??
      deck_status[, Zone := "Deck"]

      #setup initial game status

      track <- new_game_found[GAME_ID == new_game_id  & TOURNAMENT_NM == tour_name, mean(TRACK_ID)]
      srv$game_status <- start_game(cycler_ids ,track, STG_TRACK_PIECE, STG_TRACK)
      simple_gs <- srv$game_status[CYCLER_ID > 0, .(CYCLER_ID, SQUARE_ID)]
      simple_gs[, TOURNAMENT_NM := tour_name]
      simple_gs[, GAME_ID := new_game_id]
      simple_gs[, TURN_ID := -1]
      srv$gs_simple <- simple_gs
      dbWriteTable(con, "GAME_STATUS", simple_gs, row.names = FALSE, append = TRUE)

    }
  } else {
    returnV <- NULL
    returnV
  }

})

observe({

#THIS ONE MONITORS IF WE HAVE FINISHED BREAKAWAY BETTING
  req(srv$gs_simple)
  ####DEP
  breakaway_bet_data()
  ########DEPD#############

  #how many inputs needed?
  new_game_found <- tournament_data_reactive()
  if (nrow(new_game_found) > 0) {
    max_row <- new_game_found[, .N]
    tour_name <- new_game_found[max_row, TOURNAMENT_NM]
    new_game_id <- new_game_found[max_row, GAME_ID]
    track_id_input <- new_game_found[max_row, TRACK_ID]
    player_count <- new_game_found[TOURNAMENT_NM == tour_name & GAME_ID == new_game_id, .N] / 2
    ba_data <-   breakaway_bet_data()
    better_players <- ba_data[TOURNAMENT_NM == tour_name & GAME_ID == new_game_id & SECOND_BET > 0 & FIRST_BET > 0, .N]

    missing_bets <- player_count - better_players
    #check that turn is -1 and everyone has bet, then execute breakaway
    turn_is_minus_one <- FALSE

    if (length(srv$gs_simple[TOURNAMENT_NM == tour_name, .N]) > 0) {
      if (srv$gs_simple[TOURNAMENT_NM == tour_name, max(TURN_ID)] == -1) {
        turn_is_minus_one <- TRUE
      }
    }

    if (missing_bets == 0 &   turn_is_minus_one) {
      #move cycers to breakaway
      #who won?

      #tie breakers are solved in favour of who is back in the grid

      ss_cyc <- srv$gs_simple[order(SQUARE_ID)][, .(CYCLER_ID)]
      ss_cyc[, tie_breaker := seq_len(.N)]

      winner_data <- ba_data[TOURNAMENT_NM == tour_name & GAME_ID == new_game_id, .(CYCLER_ID, TOTAL_BET = SECOND_BET + FIRST_BET, FIRST_BET, SECOND_BET)][order(-TOTAL_BET)]
      join_tiebreaker <- ss_cyc[winner_data, on = "CYCLER_ID"]

      #if there are 5 or 6 cyclers, then 2 breaks, otherwise 1
      count_cyc <- nrow(ss_cyc)
      if (count_cyc > 4) {
        break_away_count <- 2
      } else {
        break_away_count <- 1
      }
      winners <- join_tiebreaker[order(-TOTAL_BET, tie_breaker)][1:break_away_count]


     lane_counter <- 0
     #create game status if missing
     if (is.null(srv$game_status)) {

       srv$game_status <- suppressWarnings(create_game_status_from_simple(srv$gs_simple, track_id_input, STG_TRACK, STG_TRACK_PIECE))
     }

       for (winner_loop in winners[, CYCLER_ID]) {
         lane_counter <- lane_counter + 1

         srv$game_status <- set_cycler_position(winner_loop, 10, lane_counter, srv$game_status)
       }
     simple_gs <- srv$game_status[CYCLER_ID > 0, .(CYCLER_ID, SQUARE_ID)]
     simple_gs[, TOURNAMENT_NM := tour_name]
     simple_gs[, GAME_ID := new_game_id]
     simple_gs[, TURN_ID := 0]

     srv$gs_simple <- simple_gs

     #then update deck_status

     deck_status <- dbSelectAll("DECK_STATUS", con )[TOURNAMENT_NM == tour_name & GAME_ID == new_game_id]
     for (winner_loop in winners[, CYCLER_ID]) {

       deck_status <- play_card(cycler_id = winner_loop,
                                card_id = winners[CYCLER_ID == winner_loop, FIRST_BET],
                                current_decks_inpu = deck_status,
                                game_id = -1,
                                turn_id = 0,
                                con = NULL,
                                force = TRUE)
       deck_status <- play_card(cycler_id = winner_loop,
                                card_id = winners[CYCLER_ID == winner_loop, SECOND_BET],
                                current_decks_inpu = deck_status,
                                game_id = -1,
                                turn_id = 0,
                                con = NULL,
                                force = TRUE)
      #exh
       deck_status <-  add_exhaustion(winner_loop, deck_status, "Deck")
       deck_status <- add_exhaustion(winner_loop, deck_status, "Deck")
     }
     info_joined <- copy(deck_status)
     info_joined[, ':=' (GAME_ID = new_game_id,
                         TOURNAMENT_NM = tour_name,
                         TURN_ID = 0,
                         HAND_OPTIONS = 0)]
     dbWriteTable(con, "DECK_STATUS", info_joined, append = TRUE, row.names = FALSE)
     dbWriteTable(con, "GAME_STATUS", simple_gs, row.names = FALSE, append = TRUE)

     #clear breakaway bet cards

     dbQ(paste0('DELETE FROM BREAKAWAY_BET_CARDS WHERE TOURNAMENT_NM = "', tour_name, '"'), con)

    }
  } else {
    NULL
  }

})


output$join_tournament <- renderUI({
  con <- connDB(con, "flaimme")
 tns <-   tournament()[, .N, by = TOURNAMENT_NM][, TOURNAMENT_NM]
 selectInput(label = "Select tournament", inputId = "join_tournament", choices = tns)


})


#resolve game state after all moves are done




  react_status <- reactiveValues(phase = 0,
                                 cycler_in_turn = 0,
                                 action_data = NULL,
                                 turn = 0,
                                 last_played_card = 0,
                                 first_cycler = 0,
                                 game_status = 0,
                                 deck_status = 0,
                                 precalc_track_agg = 0,
                                 ctM_data = 0,
                                 AI_team = 0,
                                 range_joined_team = 0,
                                 game_phase = 0)

  sourcelist <- data.table(polku = c(dir("./scripts/", recursive = TRUE)))
  sourcelist[, rivi := seq_len(.N)]
  suppressWarnings(sourcelist[, kansio := strsplit(polku, split = "/")[[1]][1], by = rivi])
  sourcelist <- sourcelist[!grep("load_scripts.R", polku)]
  sourcelist[, kansio := ifelse(str_sub(kansio, -2, -1) == ".R", "root", kansio)]


  input_kansio_list <- c(
    "tab"
  )
  for(input_kansio in input_kansio_list) {
    dir_list <- sourcelist[kansio == input_kansio, polku]
    for(filename in dir_list) {
      result = tryCatch({
        print(paste0("sourced ", filename))
        source(paste0("./scripts/", filename), local = TRUE)
      }, error = function(e) {
        print(paste0("error in loading file: ", filename))
      })
    }
  }


})
