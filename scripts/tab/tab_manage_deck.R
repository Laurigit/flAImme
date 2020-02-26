
output$exhaust_numeric_input <- renderUI({

  cycler_data <- eRstartPosData()
  cyclers <- cycler_data[order(TEAM_ID, CYCLER_ID), CYCLER_ID]
  lapply(cyclers, function(cycler_id) {
    label_input <- paste0(cycler_data[CYCLER_ID == cycler_id, UI_text], " extra exhaust")
    numericInput(inputId = paste0("exhaust_inp", "_", cycler_id),
                 label = label_input,
                 value = 0,
                 min = 0,
                 step = 1)


  })
})


output$peloton_numeric_input <- renderUI({

  #check if we have a breakaway
  ba_data <- eR_startGrid()
  count_ba <- ba_data[type == "Breakaway"]

  if (nrow(count_ba) > 0) {
    #create breakaway cost ui
    ba_cyclers <- ba_data[type == "Breakaway", CYCLER_ID]

    lapply(ba_cyclers, function(cycler_id) {
      label_input <- paste0(ba_data[CYCLER_ID == cycler_id, UI_text], " first bid card")
      label_input2 <- paste0(ba_data[CYCLER_ID == cycler_id, UI_text], " second bid card")
      fluidRow(
      numericInput(paste0("bid_one_", cycler_id),
                   label = label_input,
                   value = 2,
                   min = 2,
                   max = 9
                   ),
      numericInput(paste0("bid_two_", cycler_id),
                   label = label_input2,
                   value = 2,
                   min = 2,
                   max = 9
      ))})

    }


})

eR_startGrid <- eventReactive(input$continue_to_deck_handling, {
  required_data(c("STG_TRACK_PIECE", "STG_TRACK"))
  temp_track <- create_track_table(input$select_track, STG_TRACK_PIECE, STG_TRACK)
  start_width <- temp_track[START == 1, .N]

  grid_order <- data.table(UI_text = dragulaValue(input$dragula)$cyclersInput)
  grid_order[, grid_order := seq_len(.N)]
  grid_order[, starting_row := ceiling(grid_order / start_width)]
  grid_order[, starting_lane := seq_len(.N), by = starting_row]
  grid_order[, type := "Grid"]
  peloton_order <- data.table(UI_text = dragulaValue(input$dragula)$cyclersPeloton)
  peloton_order[, starting_lane := seq_len(.N)]
  peloton_order[, starting_row := -4]
  peloton_order[, type := "Breakaway"]
  sscols_peloton <- peloton_order[, .(UI_text, starting_row, starting_lane, type)]
  sscols_gris <- grid_order[, .(UI_text, starting_row, starting_lane, type)]
  append <- rbind(sscols_peloton, sscols_gris)
  #get ui_names back to cycler_ids
  join_ui_to_cycid <- eRstartPosData()[append, on = "UI_text"]
  #get slots
  #create temp track
  join_ui_to_cycid

  # input_STARTUP_DATA <- data.table(CYCLER_ID = c(1,2,3,4,5,6,7,8),
  #
  #                                  exhaust = c(0, 0, 0, 0, 0, 0,0,0),
  #                                  starting_row =   c(1, 1, 2, 2, 3, 3,4,4),
  #                                  starting_lane = c(1,2, 1, 2, 1, 2,1,2))

})


observeEvent(input$save_and_start, {

  #gather data from manage deck
  #exhaust

required_data(c("STG_TRACK", "STG_TRACK_PIECE", "ADM_CYCLER_DECK"))
  grid_data <- eR_startGrid()
  grid_data[, exhaust := input[[paste0("exhaust_inp_", CYCLER_ID)]], by = CYCLER_ID]

  #resolve breakaway
  grid_data[, ba_bid1 := input[[paste0("bid_one_", CYCLER_ID)]], by = CYCLER_ID]
  grid_data[, ba_bid2 := input[[paste0("bid_two_", CYCLER_ID)]], by  = CYCLER_ID]
  ba_data <- grid_data[,. (CYCLER_ID, ba_bid1, ba_bid2)]

  melt <- melt.data.table(ba_data, id.vars = c("CYCLER_ID"))
  renamed <- melt[, .(CYCLER_ID, MOVEMENT = value)][!is.na(MOVEMENT)]
  ss_input <- grid_data[, .(CYCLER_ID, exhaust, starting_row, starting_lane)]
  game_status <- start_game(ss_input, as.numeric(input$select_track), STG_TRACK_PIECE, STG_TRACK)
  deck_status <- create_decks(ss_input[, CYCLER_ID], ADM_CYCLER_DECK, ss_input[, exhaust], renamed)
  browser()
print(game_status)
print(deck_status[CYCLER_ID == 1])
})


