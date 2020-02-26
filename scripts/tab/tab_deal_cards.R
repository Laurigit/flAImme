


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
  react_status$deck_status <- draw_cards_manual_input(eR_next_cycler() , react_status$deck_status, fix_exhaust, 4)






  if (react_status$phase == 1) {
    move_first_cycler <- eR_next_cycler()
    #start COMPUTING
    card_options_in_hand <- smart_cards_options(react_status$deck_status[CYCLER_ID == eR_next_cycler() & Zone == "Hand", unique(MOVEMENT)], react_status$precalc_track_agg, move_first_cycler)
    if (length(card_options_in_hand) == 1) {
      move_amount <- card_options_in_hand
    } else {

      # print(zoom(game_status))

      phase_1_simul <-  two_phase_simulation_score(react_status$game_status, react_status$deck_status, react_status$AI_team, STG_CYCLER,
                                                   react_status$turn, react_status$ctM_data, react_status$precalc_track_agg,
                                                   react_status$range_joined_team,
                                                   card_options = card_options_in_hand, cycler_id = move_first_cycler,
                                                   phase_one_actions = NULL,
                                                   simul_rounds = 1,
                                                   simulate_until_stopped = TRUE)
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
    second_cycler <- eR_next_cycler()

    card_options_in_hand_p2 <- smart_cards_options(react_status$deck_status[CYCLER_ID == second_cycler & Zone == "Hand", unique(MOVEMENT)],
                                                   react_status$pre_aggr_game_status, second_cycler)
    if (length(card_options_in_hand_p2) == 1) {
      move_amount_p2 <- card_options_in_hand_p2
    } else {


      phase_one_actions <-  played_cards_data[TURN_ID == turn_id & MOVEMENT & PHASE == 1,. (CYCLER_ID, MOVEMENT, phase = PHASE)]

      phase_2_simul <-  two_phase_simulation_score(react_status$game_status, react_status$deck_status, react_status$AI_team,
                                                   STG_CYCLER, react_status$turn, react_status$ctM_data, react_status$precalc_track_agg,
                                                   react_status$range_joined_team,
                                                   card_options = card_options_in_hand_p2, cycler_id = second_cycler,
                                                   phase_one_actions = phase_one_actions,
                                                   simul_rounds = 1)

      simul_rs_p2 <-  simulate_and_scores_phase_2(phase_2_simul, STG_CYCLER, react_status$AI_team, second_cycler)
      move_amount_p2 <-  simul_rs_p2[, MOVEMENT]
    }
    move_cyc <- simul_rs_p2[, CYCLER_ID]


    #make sure exhaust is played if possible
    played_card_id <- react_status$deck_status[CYCLER_ID == second_cycler & MOVEMENT == move_amount_p2, min(CARD_ID)]
    react_status$action_data[CYCLER_ID  == move_cyc & TURN_ID == react_status$turn, ':=' (MOVEMENT = move_amount_p2,
                                                                                   PHASE = react_status$phase,
                                                                                   CARD_ID = played_card_id)]


}

  react_status$last_played_card <- move_amount
  hide("show_card_text")


})


output$which_first <- renderText({
  required_data("ADM_CYCLER_INFO")
  if (react_status$phase == 0) {
    res <- "Thinking"
  } else if (react_status$phase == 1) {
    res <- ADM_CYCLER_INFO[CYCLER_ID == eR_next_cycler(), UI_text]
  }

})
