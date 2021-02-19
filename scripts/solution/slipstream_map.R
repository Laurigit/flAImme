#input_track <- 13

slipstream_map <- function(input_track,  STG_TRACK_PIECE, STG_TRACK) {

  mappi <-  create_track_table(input_track, STG_TRACK_PIECE, STG_TRACK, extra_slots_to_end = 5)
  mappi[, SLIPSTREAM_POSSIBLE_TO_GIVE_OR_GET := ifelse(PIECE_ATTRIBUTE  %in% c("N", "A", "S"), 1, 0)]
  mappi[, SLIPSTREAM_POSSIBLE_TO_GET := ifelse(SLIPSTREAM_POSSIBLE_TO_GIVE_OR_GET == 1 & shift(SLIPSTREAM_POSSIBLE_TO_GIVE_OR_GET, type = "lead", n = 2) == 1, 1, 0)]
  mappi[, SLIPSTREAM_POSSIBLE_TO_GET := ifelse(is.na(SLIPSTREAM_POSSIBLE_TO_GET), 1, SLIPSTREAM_POSSIBLE_TO_GET)]
  mappi[, ':=' (ORIG_GAME_SLOT_ID = GAME_SLOT_ID,
               ORIG_LANE = LANE_NO)]
  lane_slot_map <- mappi[, .(SLIP_GAME_SLOT_ID = GAME_SLOT_ID - 1, SLIP_LANE_NO = LANE_NO, NEW_SQUARE_ID = SQUARE_ID)]

  #joinmap
  join_map <- lane_slot_map[mappi, on = .(SLIP_GAME_SLOT_ID == GAME_SLOT_ID, SLIP_LANE_NO ==  LANE_NO)]
  result <- join_map[, .(SQUARE_ID, NEW_SQUARE_ID, SLIPSTREAM_POSSIBLE_TO_GET, NEW_GAME_SLOT_ID = SLIP_GAME_SLOT_ID)]
  result[is.na(NEW_SQUARE_ID), SLIPSTREAM_POSSIBLE_TO_GET := 0]
  return(result)
}
