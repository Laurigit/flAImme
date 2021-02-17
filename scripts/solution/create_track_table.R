
# required_data(c("STG_TRACK_PIECE", "STG_TRACK", "))
# input_track <- 2
create_track_table <- function(input_track, STG_TRACK_PIECE, STG_TRACK, force_lanes = NULL, extra_slots_to_end = 10) {

  currtrack <- STG_TRACK[input_track == TRACK_ID, TRACK_PIECE_VECTOR]
  splitted_track <- data.table(TRACK_PIECE_ID = strsplit(currtrack, split = "")[[1]])
  splitted_track[, order := seq_len(.N)]

  sscols <- STG_TRACK_PIECE[, .(TRACK_PIECE_ID, LANES, PIECE_ATTRIBUTE, START, FINISH)]

  #Game_slot_id	Lane_1	Lane_2	Lane_3	Attribue	Finish

  joinaa_normi <- sscols[splitted_track, on = "TRACK_PIECE_ID"]
  if(!is.null(force_lanes)) {
    joinaa_normi[, LANES := force_lanes]
  }
  joinaa <- rbind(joinaa_normi, data.table(TRACK_PIECE_ID = "extra", LANES = 12, PIECE_ATTRIBUTE = "N", START = 0, FINISH = 0, EXTRA = rep(1, extra_slots_to_end)),  fill = TRUE)
  joinaa[, ':=' (GAME_SLOT_ID = seq_len(.N),
                 order = NULL)]

  row_rep <- joinaa[rep(1:.N,LANES)][,LANE_NO:=1:.N,by=GAME_SLOT_ID]
  # kaadettu <- dcast.data.table(row_rep, TRACK_PIECE_ID + PIECE_ATTRIBUTE + START + FINISH + GAME_SLOT_ID ~ Indx, value.var = "dcast_value")
  #colnames(kaadettu)[(length(kaadettu) - max_lanes + 1):length(kaadettu)] <- laneCols
  sort <- row_rep[order(GAME_SLOT_ID, -LANE_NO)]
  sort[, ':=' (TRACK_PIECE_ID = NULL,
               LANES = NULL,
               SQUARE_ID = seq_len(.N),
               CYCLER_ID = 0)]
  sort <- slots_out_of_mountains(sort)
  sort <- slots_out_of_mountains_to_track(sort)
  sort[, MINIMUM_MOVEMENT := ifelse(PIECE_ATTRIBUTE == "A", 5, ifelse(PIECE_ATTRIBUTE == "S", 4, 2))]


  return(sort)
}
