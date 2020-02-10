phase2_slot_score <- function(game_status, phase_two_cyclers, deck_status, range_data) {
copy_game <- copy(game_status)

copy_deck <- copy(deck_status)
move_options <- range_data[CYCLER_ID %in% phase_two_cyclers, .(MOVEMENT, CYCLER_ID, new_slot)]


agg_data <- precalc_track(copy_game)

aggr_occupied_slots <- agg_data$cycler_pos[CYCLER_ID %in% phase_two_cyclers, .N, by = cycler_pos][, cycler_pos]

agg_data$aggr_to_slots[GAME_SLOT_ID %in% aggr_occupied_slots, occupied := TRUE]
agg_data$aggr_to_slots[!GAME_SLOT_ID %in% aggr_occupied_slots, occupied := FALSE]
agg_data$aggr_to_slots[, sl_slot := ifelse(slipstream_possible == 1 & shift(occupied, type = "lead",n = 2) == TRUE
                                           & shift(occupied, type = "lead",n = 1) != TRUE, 1, 0)]
agg_data$aggr_to_slots[, sl_slot := ifelse(is.na(sl_slot), 0, sl_slot)]
agg_data$aggr_to_slots[, no_exhaust := ifelse(shift(occupied, type = "lead", n = 1) == 1 | sl_slot == 1, 1, 0)]
agg_data$aggr_to_slots[, no_exhaust := ifelse(is.na(no_exhaust), 0, no_exhaust)]
sscols <- agg_data$aggr_to_slots[, .(GAME_SLOT_ID, score = no_exhaust + sl_slot)]

#join score to options
joined_score <- sscols[move_options, on = .(GAME_SLOT_ID = new_slot)]
res_cols <- joined_score[,. (CYCLER_ID, MOVEMENT, score)]

return(res_cols)
#  move_cycler(game_status, 7, 9)
}
