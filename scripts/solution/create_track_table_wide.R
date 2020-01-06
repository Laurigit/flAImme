
# required_data(c("STG_TRACK_PIECE", "STG_TRACK"))
# input_track <- 2
create_track_table_wide <- function(input_track, STG_TRACK_PIECE, STG_TRACK) {

  currtrack <- STG_TRACK[input_track == TRACK_ID, TRACK_PIECE_VECTOR]
  splitted_track <- data.table(TRACK_PIECE_ID = strsplit(currtrack, split = ",")[[1]])
  splitted_track[, order := seq_len(.N)]

  sscols <- STG_TRACK_PIECE[, .(TRACK_PIECE_ID, LANES, PIECE_ATTRIBUTE, START, FINISH)]

  #Game_slot_id	Lane_1	Lane_2	Lane_3	Attribue	Finish

  joinaa <- sscols[splitted_track, on = "TRACK_PIECE_ID"]
  joinaa[, ':=' (GAME_SLOT_ID = seq_len(.N),
                 order = NULL,
                 dcast_value = 0)]
  max_lanes <- joinaa[, max(LANES)]

  laneCols <- paste0("Lane", 1:max_lanes)
  row_rep <-joinaa[rep(1:.N,LANES)][,Indx:=1:.N,by=GAME_SLOT_ID]
  kaadettu <- dcast.data.table(row_rep, TRACK_PIECE_ID + PIECE_ATTRIBUTE + START + FINISH + GAME_SLOT_ID ~ Indx, value.var = "dcast_value")
  colnames(kaadettu)[(length(kaadettu) - max_lanes + 1):length(kaadettu)] <- laneCols

  kaadettu[, ':=' (TRACK_PIECE_ID = NULL)]
  sort <- kaadettu[order(GAME_SLOT_ID)]
return(sort)
}
