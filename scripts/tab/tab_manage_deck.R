


observeEvent(input$start_game, {
required_data(c("STG_TRACK_PIECE", "STG_TRACK"))
temp_track <- create_track_table(input$select_track, STG_TRACK_PIECE, STG_TRACK)
start_width <- temp_track[START == 1, .N]

grid_order <- data.table(UI_text = dragulaValue(input$dragula)$cyclersInput)
grid_order[, grid_order := seq_len(.N)]
grid_order[, starting_row := ceiling(grid_order / start_width)]
grid_order[, starting_lane := seq_len(.N), by = starting_row]
peloton_order <- data.table(UI_text = dragulaValue(input$dragula)$cyclersPeloton)
peloton_order[, starting_lane := seq_len(.N)]
peloton_order[, starting_row := -4]
sscols_peloton <- peloton_order[, .(UI_text, starting_row, starting_lane)]
sscols_gris <- grid_order[, .(UI_text, starting_row, starting_lane)]
append <- rbind(sscols_peloton, sscols_gris)
#get ui_names back to cycler_ids
join_ui_to_cycid <- eRstartPosData()[append, on = "UI_text"]
#get slots
#create temp track


input_STARTUP_DATA <- data.table(CYCLER_ID = c(1,2,3,4,5,6,7,8),

                                 exhaust = c(0, 0, 0, 0, 0, 0,0,0),
                                 starting_row =   c(1, 1, 2, 2, 3, 3,4,4),
                                 starting_lane = c(1,2, 1, 2, 1, 2,1,2))
})
