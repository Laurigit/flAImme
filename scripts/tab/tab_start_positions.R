# required_data("STG_TRACK_PIECE")
# input_STARTUP_DATA <- data.table(CYCLER_ID = c(1,2,3,4,5,6,7,8),
#                                  PLAYER_ID = c(1,1,2,2,3,3,4,4),
#                                  exhaust = c(0, 0, 0, 0, 0, 0,0,0),
#                                  starting_row =   c(1, 1, 2, 2, 3, 3,4,4),
#                                  starting_lane = c(1,2, 1, 2, 1, 2,1,2))
#
# game_status <- start_game(used_startup_data,
#                           input$select_track, STG_TRACK_PIECE, eR_TRACK())




output$cyclersInput <- renderUI({


  lapply(eRstartPosData()[, UI_text],
         function(teksti) { tags$h3(drag = teksti,teksti)})
})

output$cyclersPeloton <- renderUI({

  div()
})

observeEvent(input$continue_to_deck_handling, {
  updateTabItems(session, "sidebarmenu", selected = "tab_start_positions")


})



eRstartPosData <- eventReactive(input$go_to_start_position_tab, {
  #CYCLER_ID, TYPE, COLOR,
required_data(c("STG_CYCLER", "STG_CYCLER_TYPE", "STG_TEAM"))

  join_type_and_team <- STG_CYCLER[STG_CYCLER_TYPE, on = "CYCLER_TYPE_ID"]
  join_team  <- STG_TEAM[join_type_and_team, on = "TEAM_ID"]


  loop_input <- c(input$red_setup,input$blue_setup,  input$black_setup,
                  input$green_setup,input$purple_setup,input$white_setup
                  )
  game_setup_data <- data.table(TEAM_ID = 1:6, status = "")
  loop_counter <- 0

  for(loop in loop_input) {
    loop_counter <- loop_counter + 1
    game_setup_data[TEAM_ID == loop_counter, status := loop]
  }

  join_status <- game_setup_data[join_team, on = "TEAM_ID"]
  join_status[, UI_text :=  paste0(TEAM_COLOR, "-", CYCLER_TYPE_NAME)]

  filter <- join_status[status != "Not playing"]
  filter
})
