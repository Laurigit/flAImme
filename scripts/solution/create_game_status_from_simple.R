create_game_status_from_simple <- function(simple_gs, track_id, STG_TRACK, STG_TRACK_PIECE) {

 new_track <- create_track_table(track_id, STG_TRACK_PIECE, STG_TRACK)
#set positions

new_track[, CYCLER_ID := as.integer(CYCLER_ID)]
sscols_simple <- simple_gs[, .(CYCLER_NEW = CYCLER_ID, SQUARE_ID)]
join_simple <- sscols_simple[new_track, on = "SQUARE_ID"]
join_simple[, CYCLER_ID := ifelse(!is.na(CYCLER_NEW), CYCLER_NEW, CYCLER_ID)]
join_simple[, CYCLER_NEW := NULL]
#new_track[SQUARE_ID %in% simple_gs[, SQUARE_ID], CYCLER_ID := simple_gs[, CYCLER_ID]]
#new_track[CYCLER_ID > 0]
 return(join_simple)
}
