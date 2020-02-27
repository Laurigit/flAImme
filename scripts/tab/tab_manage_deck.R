
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
  sscols_gris <- grid_order[, .(UI_text, starting_row, starting_lane, type)]
  if (!is.null(dragulaValue(input$dragula)$cyclersPeloton)) {
  peloton_order <- data.table(UI_text = dragulaValue(input$dragula)$cyclersPeloton)
  peloton_order[, starting_lane := seq_len(.N)]
  peloton_order[, starting_row := -4]
  peloton_order[, type := "Breakaway"]
  sscols_peloton <- peloton_order[, .(UI_text, starting_row, starting_lane, type)]
  } else {
    sscols_peloton <- sscols_gris[1 == 0]
  }

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


link_reactive <- reactiveValues(value = 0)

observeEvent(input$save_and_start, {

  #gather data from manage deck
  #exhaust

required_data(c("STG_TRACK", "STG_TRACK_PIECE", "ADM_CYCLER_DECK"))
  grid_data <- eR_startGrid()
  grid_data[, exhaust := input[[paste0("exhaust_inp_", CYCLER_ID)]], by = CYCLER_ID]

  #resolve breakaway
  if (!is.null(dragulaValue(input$dragula)$cyclersPeloton)) {
  grid_data[, ba_bid1 := input[[paste0("bid_one_", CYCLER_ID)]], by = CYCLER_ID]
  grid_data[, ba_bid2 := input[[paste0("bid_two_", CYCLER_ID)]], by  = CYCLER_ID]
  ba_data <- grid_data[,. (CYCLER_ID, ba_bid1, ba_bid2)]

  melt <- melt.data.table(ba_data, id.vars = c("CYCLER_ID"))
  renamed <- melt[, .(CYCLER_ID, MOVEMENT = value)][!is.na(MOVEMENT)]
  } else {
    renamed <- NULL
  }
  ss_input <- grid_data[, .(CYCLER_ID, exhaust, starting_row, starting_lane)]
  react_status$game_status <- start_game(ss_input, as.numeric(input$select_track), STG_TRACK_PIECE, STG_TRACK)

  react_status$deck_status <- create_decks(ss_input[, CYCLER_ID], ADM_CYCLER_DECK, ss_input[, exhaust], renamed)
  react_status$game_status <- slots_out_of_mountains( react_status$game_status)
  react_status$game_status <- slots_out_of_mountains_to_track( react_status$game_status)
    print(zoom(  react_status$game_status ))
  cyclers <- eRstartPosData()[status != "Not playing", CYCLER_ID]
  move_data <- data.table(CYCLER_ID = cyclers, MOVEMENT = 0, CARD_ID = 0, key_col = "1")
  turn_amount <- 25
  played_cards_data <- merge(x = data.table(TURN_ID = 1:turn_amount, key_col = "1"), y = move_data, by = "key_col", all = TRUE, allow.cartesian = TRUE)
  played_cards_data[, key_col := NULL]
  played_cards_data[, PHASE := 0]
  react_status$action_data <- played_cards_data


  updateTabItems(session, "sidebarmenu", selected = "tab_deal_cards")
  react_status$turn <- 1
  react_status$phase <- 1
  #we should start simulating asap
  react_status$AI_team <- eRstartPosData()[status %in% c("AI", "AI autocards"), max(TEAM_ID)]
  react_status$AI_cyclers <- STG_CYCLER[TEAM_ID == react_status$AI_team, CYCLER_ID]

  react_status$AI_cards <- eRstartPosData()[status %in% c("AI", "AI autocards"), status][1]
  react_status$game_phase <- 1

})


observeEvent(react_status$game_phase, {

  if (react_status$game_phase == 1) {
    #calculate first cycler

    link_reactive$value <-  link_reactive$value + 1

  } else if (react_status$game_phase == 2){
    #wait for cards or auto deal
    if (react_status$AI_cards == "AI autocards") {
      for (loop_cyc in  react_status$AI_cyclers) {
        react_status$deck_status <- draw_cards(loop_cyc,   react_status$deck_status, 4, FALSE)
      }
      react_status$game_phase <- 3
      updateTabItems(session, "sidebarmenu", selected = "tab_play_card")
    }
  } else if (react_status$game_phase == 3) {
    choose_and_play$now <-  choose_and_play$now + 1

  } else if (react_status$game_phase == 4) {
    #"calculate" next cycler
    link_reactive$value <-  link_reactive$value + 1
  } else if (react_status$game_phase == 5) {
    #wait for cards or auto deal
    if (react_status$AI_cards == "AI autocards") {
      for (loop_cyc in  react_status$AI_cyclers) {
        react_status$deck_status <- draw_cards(loop_cyc,   react_status$deck_status, 4, FALSE)
      }
      react_status$game_phase <- 6
      updateTabItems(session, "sidebarmenu", selected = "tab_play_card")
    }
  } else if (react_status$game_phase == 6) {
    # calculate move and wait for human input
    choose_and_play$now <-  choose_and_play$now + 1
  }
},ignoreInit = )

state <- reactiveValues(next_cycler = 0)

#AI <- reactiveValues(ready = FALSE)
observe({
  ###DEP
  print(link_reactive$value )

  ####
  isolate({
required_data("ADM_AI_CONF")
  required_data("STG_CYCLER")
  if (react_status$phase == 1) {
  react_status$precalc_track_agg <- precalc_track(react_status$game_status )
  ctM_res <- cyclers_turns_MOVEMEMENT_combs(con, ADM_OPTIMAL_MOVES, react_status$game_status, react_status$deck_status, react_status$precalc_track_agg)
  ADM_OPTIMAL_MOVES <- ctM_res$new_ADM_OPT
  react_status$ctM_data <- ctM_res$ctM_data

  react_status$range_joined_team <- calc_move_range(react_status$game_status, react_status$deck_status, react_status$ctM_data, STG_CYCLER)

  simult_list_res <-  two_phase_simulation_score(react_status$game_status, react_status$deck_status, react_status$AI_team,
                                                 STG_CYCLER, react_status$turn, react_status$ctM_data, react_status$precalc_track_agg,
                                                 react_status$range_joined_team,
                                                 card_options = NULL, cycler_id = NULL, phase_one_actions = NULL,
                                                 simul_rounds = 1,
                                                 ADM_AI_CONF = ADM_AI_CONF)
  react_status$ctM_data <- simult_list_res$updated_ctm

  next_cycler <- which_cycler_to_move_first(simult_list_res$scores, STG_CYCLER, react_status$AI_team)
  react_status$game_phase <- 2
  } else if (react_status$phase == 2) {
    required_data("ADM_CYCLER_INFO")
    next_cycler <- ADM_CYCLER_INFO[TEAM_ID == ADM_CYCLER_INFO[CYCLER_ID == react_status$first_cycler, react_status$AI_team] & CYCLER_ID != react_status$first_cycler, CYCLER_ID]
    react_status$game_phase <- 5


  } else {
    next_cycler <- 0
  }
  })
  react_status$first_cycler <- next_cycler
  react_status$cycler_in_turn <- next_cycler

  state$next_cycler <- next_cycler
})

