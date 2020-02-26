#eR_choose_card(react_status$phase, {})


output$which_cycler_playing <- renderText({

  required_data("ADM_CYCLER_INFO")
  if (react_status$phase == 0) {
    res <- "Thinking"
  } else  {
    res <- ADM_CYCLER_INFO[CYCLER_ID == eR_next_cycler(), UI_text]
  }
    res
})


output$play_card_text <- renderText({

  res <- react_status$last_played_card
  res
})


observeEvent(input$show_card, {

  shinyjs::show("show_card_text")
})
