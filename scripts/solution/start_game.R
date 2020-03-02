
start_game <- function(cycler_ids, track, STG_TRACK_PIECE, STG_TRACK, force_lanes = NULL) {

#create track

tracki <-  create_track_table(track, STG_TRACK_PIECE, STG_TRACK, force_lanes)

start_row <- tracki[START == 1, max(GAME_SLOT_ID)]
#set positions
updated_status <- tracki

start_row_width <- tracki[START == 1, .N]

cycler_dt <- data.table(CYCLER_ID = cycler_ids)


cycler_dt[, grid_order := seq_len(.N)]
cycler_dt[, starting_row := ceiling(grid_order / start_row_width)]
cycler_dt[, starting_lane := seq_len(.N), by = starting_row]

for(loop in cycler_ids) {
  row_data <- cycler_dt[CYCLER_ID == loop]
updated_status <-   set_cycler_position(row_data[, CYCLER_ID],
                      start_row + 1- row_data[, starting_row],
                      row_data[, starting_lane],
                      updated_status)

}
updated_status <- slots_out_of_mountains_to_track(updated_status)
updated_status <- slots_out_of_mountains(updated_status)
return(updated_status)
}
