#tab_input_other_moves

playedt <- reactiveValues(cards = "")

output$radioGroups <- renderUI({
  #human_cyclers
  required_data("STG_TEAM")
  cycler_data <- eRstartPosData()
 # human_cyclers <- cycler_data[status == "Human", CYCLER_ID]
 human_cyclers <- cycler_data[status == "Human", UI_text]

    label_input <- "Which cycler"
    radioGroupButtons(inputId = "radio_human_cyclers",
                      label = label_input,
                      choices = sort(human_cyclers),
                      direction = "vertical",
                      individual = TRUE,
                      size = "lg",
                      width = "100px"
                      )


})


observeEvent(input$played_2, {
  playedt$cards <- paste0(playedt$cards, input$radio_human_cyclers, ": 2 <br>")
})

observeEvent(input$played_3, {
  playedt$cards <- paste0(playedt$cards, input$radio_human_cyclers, ": 3 <br>")
})

observeEvent(input$played_4, {
  playedt$cards <- paste0(playedt$cards, input$radio_human_cyclers, ": 4 <br>")
})

observeEvent(input$played_5, {
  playedt$cards <- paste0(playedt$cards, input$radio_human_cyclers, ": 5 <br>")
})

observeEvent(input$played_6, {
  playedt$cards <- paste0(playedt$cards, input$radio_human_cyclers, ": 6 <br>")
})

observeEvent(input$played_7, {
  playedt$cards <- paste0(playedt$cards, input$radio_human_cyclers, ": 7 <br>")
})

observeEvent(input$played_9, {
  playedt$cards <- paste0(playedt$cards, input$radio_human_cyclers, ": 9 <br>")
})

observeEvent(input$played_E, {
  playedt$cards <- paste0(playedt$cards, input$radio_human_cyclers, ": E <br>")
})

output$cards_playedt <- renderUI({
  HTML(playedt$cards)

})

observeEvent(input$undo_played, {
  playedt$cards <-  ""
})


observeEvent(input$save_played_cards, {
  required_data("ADM_CYCLER_INFO")
  text <- playedt$cards
  splitted <- str_split(text, "<br>")
  dt_rest <- data.table(splitted[[1]])
  dt_rest[, row_id := seq_len(.N)]
  dt_rest[, str_split(V1, ":")]
  dt_rest[, c("UI_text", "MOVEMENT") := tstrsplit(V1, ":", fixed = TRUE), by = row_id]
  sscols <- dt_rest[!is.na(MOVEMENT), .(MOVEMENT = as.numeric(MOVEMENT), UI_text = str_trim(UI_text))]
  sscols_info <- ADM_CYCLER_INFO[,. (CYCLER_ID, UI_text)]

  join_info <- sscols_info[sscols, on = "UI_text"]
  moving_cyclers <- join_info[, CYCLER_ID]

    for (loop_update in moving_cyclers) {
    #make sure exhaust is played
    move_amt <- join_info[CYCLER_ID == loop_update, MOVEMENT]
    played_card_id <- react_status$deck_status[CYCLER_ID == loop_update & MOVEMENT == move_amt, min(CARD_ID)]
    react_status$action_data[TURN_ID == react_status$turn &
                               CYCLER_ID == loop_update, ':=' (MOVEMENT = move_amt,
                                                                  PHASE = react_status$phase,
                                                               CARD_ID = played_card_id)]
  }



  #start moving cyclers

  phase_cyclers_in_move_order <- create_move_order_vec(game_status, moving_cyclers)
  for(loop_move in phase_cyclers_in_move_order) {

    row_data <- played_cards_data[TURN_ID == react_status$turn & loop_move == CYCLER_ID & PHASE == react_status$turn]

    react_status$deck_status <- play_card(cycler_id = loop_move,
                             card_id = row_data[, CARD_ID],
                             current_decks = deck_status, 1, 1, FALSE)
    react_status$game_status <- move_cycler(game_status, loop_move, movement = row_data[, MOVEMENT])

  }
  print(zoom(game_status))
  #if phase two
  if (react_status$phase == 2) {
    game_status <- apply_slipstream(react_status$game_status)
    deck_status <- apply_exhaustion(react_status$deck_status, react_status$game_status)
    react_status$turn <- react_status$turn + 1
    react_status$phase <- 1
  } else {

    react_status$phase <- 2
  }
})


