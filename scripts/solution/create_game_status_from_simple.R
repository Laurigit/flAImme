create_game_status_from_simple <- function(simple_gs, track_id, STG_TRACK, STG_TRACK_PIECE) {
 new_track <- create_track_table(track_id, STG_TRACK_PIECE, STG_TRACK)
#set positions
 new_track[SQUARE_ID %in% simple_gs[, SQUARE_ID], CYCLER_ID := simple_gs[, CYCLER_ID]]

 return(new_track)
}
