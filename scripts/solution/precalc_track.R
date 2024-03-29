#precalc map for optimization

precalc_track <- function(game_status) {

aggr_to_slots <- game_status[, .N, by = .(GAME_SLOT_ID, PIECE_ATTRIBUTE, FINISH, MAXIMUM_MOVEMENT, MINIMUM_MOVEMENT)]

all_cycler_pos <- game_status[CYCLER_ID > 0, .(cycler_pos = max(GAME_SLOT_ID)), by = CYCLER_ID]

aggr_to_slots[, key := "1"]
finish_slot <- aggr_to_slots[FINISH == 1, GAME_SLOT_ID]

aggr_to_slots[, TRACK_LEFT := convert_track_left_to_text(aggr_to_slots, GAME_SLOT_ID, finish_slot), by = .(GAME_SLOT_ID)]

#aggr_to_slots[, restricted_v := start_of_restricted_movement == 1]
aggr_to_slots[, ascend_v := MINIMUM_MOVEMENT]

aggr_to_slots[, slipstream_possible := ifelse(!PIECE_ATTRIBUTE %in% c("M", "C") &
                                                !shift(PIECE_ATTRIBUTE, n = 1, type = "lead") %in%  c("M", "C")
                                              & !shift(PIECE_ATTRIBUTE, n = 2, type = "lead") %in% c("M", "C"), 1, 0)]
aggr_to_slots[, slipstream_possible := ifelse(is.na(slipstream_possible), 1, slipstream_possible)]

res_list <- NULL
#print(aggr_to_slots[1])
res_list$aggr_to_slots <- aggr_to_slots
res_list$cycler_pos <- all_cycler_pos
return(res_list)

}
