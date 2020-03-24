
luettu <- dbSelectAll("ADM_OPTIMAL_MOVES", con)

ADM_OPTIMAL_MOVES <- fix_colnames(luettu)



shinyServer(function(input, output, session) {

  con <- connDB(con, "flaimme")
  srv <- reactiveValues(game_status = NULL,
                        gs_simple = dbSelectAll("GAME_STATUS", con))

#
#   #monitor user inputs
#   read_max_game_id <- function() {
#     browser()
#     con <- connDB(con, "flaimme")
#     dbQ("SELECT max(GAME_ID) from TOURNAMENT_RESULT", con)
#   }
#
#   read_tournament_result <- function() {
#     con <- connDB(con, "flaimme")
#     dbSelectAll("TOURNAMENT_RESULT", con)
#   }


  #tour_info_data <-  my_reactivePoll(session, "TOURNAMENT_RESULT", "SELECT * FROM TOURNAMENT", 1000, con)
 tournament_data_reactive <- my_reactivePoll(session, "TOURNAMENT_RESULT", "SELECT * FROM TOURNAMENT_RESULT", 1000, con)

 breakaway_bet_data <- my_reactivePoll(session, "BREAKAWAY_BET", "SELECT * FROM BREAKAWAY_BET", 5000, con)

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
      #who one?
      winners <- ba_data[TOURNAMENT_NM == tour_name & GAME_ID == new_game_id, .(CYCLER_ID, TOTAL_BET = SECOND_BET + FIRST_BET, FIRST_BET, SECOND_BET)][order(-TOTAL_BET)][1:2]
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
browser()
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

    }
  } else {
    NULL
  }

})


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
