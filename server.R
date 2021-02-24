
luettu <- dbSelectAll("ADM_OPTIMAL_MOVES", con)

ADM_OPTIMAL_MOVES <- fix_colnames(luettu)



shinyServer(function(input, output, session) {

  con <- connDB(con, "flaimme")
  srv <- reactiveValues(game_status = NULL,
                        gs_simple = NULL,
                        track_id_input = NULL,
                        game_id = NULL,
                        turn_id = NULL)



  #tour_info_data <-  my_reactivePoll(session, "TOURNAMENT_RESULT", "SELECT * FROM TOURNAMENT", 1000, con)
 tournament_data_reactive <- my_reactivePoll(session, "TOURNAMENT_RESULT", "SELECT * FROM TOURNAMENT_RESULT", 2000, con)
 tournament <- my_reactivePoll(session, "TOURNAMENT", "SELECT * FROM TOURNAMENT", 2000, con)
 breakaway_bet_data <- my_reactivePoll(session, "BREAKAWAY_BET", "SELECT * FROM BREAKAWAY_BET", 3000, con)

 breakaway_bet_cards_data <- my_reactivePoll(session, "BREAKAWAY_BET_CARDS", "SELECT * FROM BREAKAWAY_BET_CARDS", 2000, con)

 move_fact_data_all  <- my_reactivePoll(session, "MOVE_FACT", "SELECT * from MOVE_FACT", 1000, con)

command_data <-  my_reactivePoll(session, "CLIENT_COMMANDS", "SELECT * from CLIENT_COMMANDS", 1000, con)


game_data <-  my_reactivePoll(session, "GAME", paste0('SELECT sum(CYCLER_ID) FROM GAME'), timeout = 1000, con)


move_fact_data <- reactive({
  req(move_fact_data_all())
      req(input$join_tournament)

  res <- move_fact_data_all()[GAME_ID == srv$game_id & TOURNAMENT_NM ==  input$join_tournament]
  res
})

output$join_tournament <- renderUI({
  con <- connDB(con, "flaimme")
 tns <-   tournament()[, .N, by = TOURNAMENT_NM][, TOURNAMENT_NM]
 selectInput(label = "Select tournament", inputId = "join_tournament", choices = tns)


})

observeEvent(input$join_tournament,{
#update reactivevalue based on selected tournament

  tn_data <- tournament_data_reactive()[TOURNAMENT_NM == input$join_tournament]

    new_game_id <- tn_data[, max(GAME_ID)]
    srv$track_id_input <- tn_data[GAME_ID == new_game_id, max(TRACK_ID)]

      gs_tournament_game <- dbSelectAll("GAME_STATUS", con)[TOURNAMENT_NM == input$join_tournament & GAME_ID == new_game_id]
    max_turn <- gs_tournament_game[, max(TURN_ID)]
    srv$gs_simple <-  gs_tournament_game[TURN_ID == max_turn]
    srv$turn_id <- max_turn
    srv$game_id <- new_game_id
})

deck_status_data <-  my_reactivePoll(session, "DECK_STATUS", paste0('SELECT sum(CYCLER_ID) FROM DECK_STATUS'), timeout = 1000, con)

deck_status_curr_game <- reactive({

  req(input$join_tournament)
  deck_tour <- deck_status_data()[TOURNAMENT_NM == input$join_tournament]
  find_latest_update <- deck_tour[, .N]
  game_id <- deck_tour[find_latest_update, GAME_ID]
  turn_id <- deck_tour[find_latest_update, TURN_ID]
  # hand_options <-  deck_tour[find_latest_update, HAND_OPTIONS]
  result <- deck_tour[GAME_ID == game_id]
  result
})


observe({
  tournament_result$data <- tournament_data_reactive()
})

game_status_simple <-  my_reactivePoll(session, "GAME_STATUS", paste0('SELECT sum(CYCLER_ID) FROM GAME_STATUS'), timeout = 1000, con)

tournament_result <- reactiveValues(data = NULL)

game_status <- reactive({

  if (nrow(game_status_simple()) > 0) {

    tn_data <- tournament_result$data[TOURNAMENT_NM == input$join_tournament]
    #cycler_info <- ADM_CYCLER_INFO[CYCLER_ID %in% tn_data[, CYCLER_ID]]
    max_game <- tn_data[, max(GAME_ID)]

    #create game status

    track <- tn_data[LANE == -1, max(TRACK_ID)]
    game_id <- tn_data[LANE == -1, max(GAME_ID)]
    turni <- game_status_simple()[TOURNAMENT_NM == input$join_tournament & GAME_ID == game_id, max(TURN_ID)]
    gs_curr_turn <- game_status_simple()[TOURNAMENT_NM == input$join_tournament & GAME_ID == game_id & TURN_ID == turni]
    game_status_local <- create_game_status_from_simple(gs_curr_turn, track,  STG_TRACK, STG_TRACK_PIECE)
  } else {
    NULL
  }
})
#resolve game state after all moves are done



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
