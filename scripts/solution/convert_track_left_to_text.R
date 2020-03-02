convert_track_left_to_text <- function(aggr_track, calc_from_slot, finish_slot) {


  track_left <- aggr_track[GAME_SLOT_ID >= calc_from_slot & GAME_SLOT_ID <= finish_slot, paste0(PIECE_ATTRIBUTE, collapse = "")]
  return(track_left)
}
