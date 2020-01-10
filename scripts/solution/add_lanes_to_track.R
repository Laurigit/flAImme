add_lanes_to_track <- function(game_status) {

  aggr_first <- game_status[, .N, by = .(GAME_SLOT_ID, PIECE_ATTRIBUTE, START, FINISH)]
  #count cyclers
  count_c <- game_status[CYCLER_ID > 0, .N, by = CYCLER_ID][, length(CYCLER_ID)]
  aggr_first[, LANES := count_c]
  row_rep <- aggr_first[rep(1:.N,LANES)][,LANE_NO:=1:.N,by=GAME_SLOT_ID]
  sort <- row_rep[order(GAME_SLOT_ID, -LANE_NO)]
  sort[, ':=' (
    LANES = NULL,
    SQUARE_ID = seq_len(.N))]

  #join_current_game_status
  current_status <- game_status[, .(CYCLER_ID, GAME_SLOT_ID, LANE_NO)]
  join_stat <- current_status[sort, on = .(GAME_SLOT_ID, LANE_NO)]
  join_stat[, CYCLER_ID := ifelse(is.na(CYCLER_ID), 0, CYCLER_ID)]
  join_stat[, N := NULL]
  return(join_stat)
}
