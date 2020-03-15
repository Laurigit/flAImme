phase2_slot_score <- function(game_status, phase_two_cyclers, deck_status, range_data, agg_data) {
  copy_game <- copy(game_status)
  #HERE SHOULD BE SCORE WHEN HELPING TEAMMATE
  copy_deck <- copy(deck_status)
  move_options <- range_data[CYCLER_ID %in% phase_two_cyclers, .(MOVEMENT, CYCLER_ID, new_slot)]

  aggr_occupied_slots <- game_status[!CYCLER_ID %in% phase_two_cyclers & CYCLER_ID > 0, unique(GAME_SLOT_ID) ]


  sscols_track_agg <- agg_data$aggr_to_slots[, .(GAME_SLOT_ID, slipstream_possible)]

  sscols_track_agg[GAME_SLOT_ID %in% aggr_occupied_slots, occupied := TRUE]
  sscols_track_agg[!GAME_SLOT_ID %in% aggr_occupied_slots, occupied := FALSE]
  sscols_track_agg[, sl_slot := ifelse(slipstream_possible == 1 & shift(occupied, type = "lead",n = 2) == TRUE
                                       & shift(occupied, type = "lead",n = 1) != TRUE, 1, 0)]
  sscols_track_agg[, sl_slot := ifelse(is.na(sl_slot), 0, sl_slot)]
  sscols_track_agg[, no_exhaust := ifelse(shift(occupied, type = "lead", n = 1) == 1 | sl_slot == 1, 1, 0)]
  sscols_track_agg[, no_exhaust := ifelse(is.na(no_exhaust), 0, no_exhaust)]
sscols <- sscols_track_agg[, .(GAME_SLOT_ID, score = no_exhaust + sl_slot)]

#join score to options
joined_score <- sscols[move_options, on = .(GAME_SLOT_ID = new_slot)]
res_cols <- joined_score[,. (CYCLER_ID, MOVEMENT, score)]

return(res_cols)
#  move_cycler(game_status, 7, 9)
}
