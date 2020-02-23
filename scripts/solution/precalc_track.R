#precalc map for optimization

precalc_track <- function(game_status) {

aggr_to_slots <- game_status[, .N, by = .(GAME_SLOT_ID, PIECE_ATTRIBUTE, FINISH, MAXIMUM_MOVEMENT)]

all_cycler_pos <- game_status[CYCLER_ID > 0, .(cycler_pos = max(GAME_SLOT_ID)), by = CYCLER_ID]
#
# aggr_to_slots[, mountain_row := ifelse(PIECE_ATTRIBUTE == "M", 1, 0)]
# aggr_to_slots[, start_of_restricted_movement := ifelse(shift(mountain_row == 1, n = 1, type = "lead") |
#                                                          shift(mountain_row == 1, n = 2, type = "lead") |
#                                                          shift(mountain_row == 1, n = 3, type = "lead") |
#                                                          shift(mountain_row == 1, n = 4, type = "lead") |
#                                                          shift(mountain_row == 1, n = 5, type = "lead") |
#                                                          shift(mountain_row == 1, n = 6, type = "lead") | mountain_row == 1, 1, 0)]
# aggr_to_slots[, start_of_restricted_movement := ifelse(is.na(start_of_restricted_movement), 0, start_of_restricted_movement)]
aggr_to_slots[, key := "1"]



#aggr_to_slots[, restricted_v := start_of_restricted_movement == 1]
aggr_to_slots[, ascend_v := PIECE_ATTRIBUTE == "A"]

aggr_to_slots[, slipstream_possible := ifelse(PIECE_ATTRIBUTE != "M" & shift(PIECE_ATTRIBUTE, n = 1, type = "lead") != "M"
                                              & shift(PIECE_ATTRIBUTE, n = 2, type = "lead") != "M", 1, 0)]
aggr_to_slots[, slipstream_possible := ifelse(is.na(slipstream_possible), 1, slipstream_possible)]

res_list <- NULL
print(aggr_to_slots[1])
res_list$aggr_to_slots <- aggr_to_slots
res_list$cycler_pos <- all_cycler_pos
return(res_list)

}
