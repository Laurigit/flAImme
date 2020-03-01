


#input buttons

dealt <- reactiveValues(cards = "")
observeEvent(input$deal_2, {
  dealt$cards <- paste0(dealt$cards, "2")
})

observeEvent(input$deal_3, {
  dealt$cards <- paste0(dealt$cards, "3")
})

observeEvent(input$deal_4, {
  dealt$cards <- paste0(dealt$cards, "4")
})

observeEvent(input$deal_5, {
  dealt$cards <- paste0(dealt$cards, "5")
})

observeEvent(input$deal_6, {
  dealt$cards <- paste0(dealt$cards, "6")
})

observeEvent(input$deal_7, {
  dealt$cards <- paste0(dealt$cards, "7")
})

observeEvent(input$deal_9, {
  dealt$cards <- paste0(dealt$cards, "9")
})

observeEvent(input$deal_E, {
  dealt$cards <- paste0(dealt$cards, "E")
})

output$cards_dealt <- renderText({
  dealt$cards

})

observeEvent(input$undo_deal, {
  dealt$cards <-  str_sub(dealt$cards, 1, -2)
})


observeEvent(input$save_dealt_cards, {

  drawn_cards_raw  <- paste(dealt$cards, collapse = "")

  card_vec <- unlist(strsplit(drawn_cards_raw, split = ""))
  fix_exhaust <- as.numeric(gsub("E", 1, card_vec))
  updateTabItems(session, "sidebarmenu", selected = "tab_play_card")
  react_status$deck_status <- draw_cards_manual_input(state$next_cycler , react_status$deck_status, fix_exhaust, 4)

  dealt$cards <- ""
  if ( react_status$game_phase == 2) {
    react_status$game_phase <- 3
  } else if ( react_status$game_phase == 5) {
    react_status$game_phase <- 6
  }

})

choose_and_play <- reactiveValues(now = 0)

observeEvent(choose_and_play$now,{


  if (react_status$phase == 1) {
    move_first_cycler <- state$next_cycler
    #start COMPUTING

    card_options_in_hand <- smart_cards_options(react_status$deck_status[CYCLER_ID == state$next_cycler & Zone == "Hand", unique(MOVEMENT)], react_status$precalc_track_agg, move_first_cycler)
    if (length(card_options_in_hand) == 1) {
      move_amount <- card_options_in_hand
    } else {

      # print(zoom(game_status))

      phase_1_simul <-  two_phase_simulation_score(react_status$game_status, react_status$deck_status, react_status$AI_team, STG_CYCLER,
                                                   react_status$turn, react_status$ctM_data, react_status$precalc_track_agg,
                                                   react_status$range_joined_team,
                                                   card_options = card_options_in_hand, cycler_id = move_first_cycler,
                                                   phase_one_actions = NULL,
                                                   simul_rounds = 10,
                                                   simulate_until_stopped = TRUE,
                                                   ADM_AI_CONF = ADM_AI_CONF)
      react_status$ctM_data <- phase_1_simul$updated_ctm
      #  simul_res_p1 <-  simulate_and_scores_phase_1(phase_1_simul$scores, STG_CYCLER, move_first_cycler)

      simul_res_p1 <-  simulate_and_scores_phase_2(phase_1_simul, STG_CYCLER, react_status$AI_team, move_first_cycler)
      move_amount <-  simul_res_p1[, MOVEMENT]
    }
    #take min_card_id to play exhaust first
    played_card_id <- react_status$deck_status[CYCLER_ID == move_first_cycler & MOVEMENT == move_amount, min(CARD_ID)]
    react_status$action_data[CYCLER_ID  == move_first_cycler & TURN_ID == react_status$turn, ':=' (MOVEMENT = move_amount,
                                                                                                   PHASE = react_status$phase,
                                                                                                   CARD_ID = played_card_id)]

  } else if (react_status$phase == 2) {

    #who am i
    second_cycler <- state$next_cycler

    card_options_in_hand_p2 <- smart_cards_options(react_status$deck_status[CYCLER_ID == second_cycler & Zone == "Hand", unique(MOVEMENT)],
                                                   react_status$precalc_track_agg, second_cycler)
    if (length(card_options_in_hand_p2) == 1) {
      move_amount <- card_options_in_hand_p2
    } else {


      phase_one_actions <-   react_status$action_data[TURN_ID ==  react_status$turn & MOVEMENT & PHASE == 1,. (CYCLER_ID, MOVEMENT, phase = PHASE)]

      phase_2_simul <-  two_phase_simulation_score(react_status$game_status, react_status$deck_status, react_status$AI_team,
                                                   STG_CYCLER, react_status$turn, react_status$ctM_data, react_status$precalc_track_agg,
                                                   react_status$range_joined_team,
                                                   card_options = card_options_in_hand_p2, cycler_id = second_cycler,
                                                   phase_one_actions = phase_one_actions,
                                                   simul_rounds = 50,
                                                   ADM_AI_CONF = ADM_AI_CONF)

      simul_rs_p2 <-  simulate_and_scores_phase_2(phase_2_simul, STG_CYCLER, react_status$AI_team, second_cycler)
      move_amount <-  simul_rs_p2[, MOVEMENT]
    }
    move_cyc <- second_cycler


    #make sure exhaust is played if possible
    played_card_id <- react_status$deck_status[CYCLER_ID == second_cycler & MOVEMENT == move_amount, min(CARD_ID)]
    react_status$action_data[CYCLER_ID  == move_cyc & TURN_ID == react_status$turn, ':=' (MOVEMENT = move_amount,
                                                                                          PHASE = react_status$phase,
                                                                                          CARD_ID = played_card_id)]


  }

  react_status$last_played_card <- move_amount
  hide("show_card_text")


}, ignoreInit = TRUE)


output$which_first <- renderText({
  required_data("ADM_CYCLER_INFO")

  if (react_status$game_phase == 2 | react_status$game_phase == 5) {

    res <- paste0(ADM_CYCLER_INFO[CYCLER_ID == state$next_cycler, UI_text], " ", Sys.time(), " Game state: ", react_status$game_phase)
  } else {
    res <- "Calculating"
  }

})
