# required_data("STG_CYCLER")
# input_STARTUP_DATA <- data.table(CYCLER_ID = c(1,2,3,4,5,6),
#                                  PLAYER_ID = c(1,1,2,2,3,3),
#                                  exhaust = c(0, 0, 0, 0, 0, 0),
#                                  starting_row =  c(1,2,1, 3,2,3),
#                                   starting_lane = c(1, 1, 2, 1, 2, 2))
#  track <- 2

start_game <- function(input_STARTUP_DATA, track, STG_TRACK_PIECE, STG_TRACK, force_lanes = NULL) {

#create track
tracki <-  create_track_table(track, STG_TRACK_PIECE, STG_TRACK, force_lanes)
start_row <- tracki[START == 1, max(GAME_SLOT_ID)]
#set positions
updated_status <-tracki
for(loop in 1:nrow(input_STARTUP_DATA)) {
  row_data <- input_STARTUP_DATA[loop]
updated_status <-   set_cycler_position(row_data[, CYCLER_ID],
                      start_row + 1- row_data[, starting_row],
                      row_data[, starting_lane],
                      updated_status)
}

return(updated_status)
}
