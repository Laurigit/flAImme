output$select_track <- renderUI({
#required_data("STG_TRACK")

  data_used <-   eR_TRACK()

  #create named list
  my_list <- data_used[, TRACK_ID]
  names(my_list) <- data_used[, TRACK_NAME]

  selectInput(inputId = "select_track",
              label = "Select track",
              choices = my_list)

})


observeEvent(input$go_to_add_track_tab, {
  updateTabItems(session, "sidebarmenu", selected = "tab_add_track")
})
