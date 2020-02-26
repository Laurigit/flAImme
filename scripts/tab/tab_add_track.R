observeEvent(input$save_custom_track, {

sql_ins <- paste0('INSERT INTO TRACK (TRACK_NAME, TRACK_PIECE_VECTOR) VALUES (\'',
                  input$new_track_name, '\', \'',
                  input$track_letters,
                  '\')')
dbQ(sql_ins, con)
updateTextInput(session,
                inputId = "new_track_name",
                value = "")
updateTextInput(session,
                inputId = "track_letters",
                value = "")
updateTabItems(session, "sidebarmenu", selected = "tab_game_setup")
})


eR_TRACK <- eventReactive(input$save_custom_track, {
  required_data("STG_TRACK", force_update = TRUE)
  STG_TRACK
}, ignoreNULL = FALSE)
