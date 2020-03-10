
#liikuta vihut move _cycler
#jätä alkupaikat paikoilleen[ max 1 per tiimi]
#änge väliin

phaseless_simulation <- function(game_status, my_first_cycler,
                                 my_second_cycler,
                                 my_team, STG_CYCLER, move_simul_data,
                                 return_pos_vec = FALSE) {

cop_game <- copy(game_status)


move_order <- cop_game[CYCLER_ID > 0, .(CYCLER_ID, SQUARE_ID, GAME_SLOT_ID)][order(-SQUARE_ID)]

#jion team
join_team <- STG_CYCLER[move_order, on = "CYCLER_ID"]
join_team_reverse <- join_team[order(-SQUARE_ID)]#[TEAM_ID != my_team]
join_team_reverse[, move_order_by_team := seq_len(.N), by = TEAM_ID]
join_team_reverse[, move_order_by_team := ifelse(CYCLER_ID == my_first_cycler, 1, ifelse(CYCLER_ID == my_second_cycler, 2, move_order_by_team))]
priority_order_cyclers <- join_team_reverse[order(move_order_by_team, -SQUARE_ID), CYCLER_ID]

#the cyclers ahead of me expect my team mate
first_phase_cyclers <-  join_team_reverse[order(move_order_by_team, -SQUARE_ID)][SQUARE_ID > move_order[CYCLER_ID == my_first_cycler, SQUARE_ID] & CYCLER_ID != my_second_cycler, CYCLER_ID]





my_move <- move_simul_data[CYCLER_ID == my_first_cycler, MOVEMENT]

my_new_slot <- move_cycler(cop_game, my_first_cycler, my_move, FALSE,
                           ignore_block = TRUE, TRUE, TRUE)

#try to choose the teams from first phase cyclers as close as possible to my new slot
#teams with more than 1 option
option_teams <- join_team_reverse[CYCLER_ID %in% first_phase_cyclers, .N, by = TEAM_ID][N > 1, TEAM_ID]
option_cycylers <- join_team_reverse[TEAM_ID %in% option_teams, CYCLER_ID]
potential_slots <- move_simul_data[CYCLER_ID %in% option_cycylers]

if (nrow(potential_slots) > 0) {
potential_slots[, new_pos := move_cycler(cop_game, CYCLER_ID, MOVEMENT, FALSE, ignore_block = TRUE, return_numeric_position = TRUE), by = CYCLER_ID]
for (loop_cyclers_distance in option_cycylers) {
  potential_slots[CYCLER_ID %% 2 == 1, ':=' (distance_to_my_cycler_if_moving_min_cycler = my_new_slot - new_pos,
                                             distance_to_my_cycler_if_moving_max_cycler = my_new_slot - curr_pos)]
  potential_slots[CYCLER_ID %% 2 == 0, ':=' (distance_to_my_cycler_if_moving_min_cycler = my_new_slot - curr_pos,
                                             distance_to_my_cycler_if_moving_max_cycler = my_new_slot - new_pos)]

}

#adjust so that rather try to be ahead than behind
potential_slots[, score_to_my_cycler_if_moving_min_cycler := 2 ^ (20 - ifelse(distance_to_my_cycler_if_moving_min_cycler > 0, abs(distance_to_my_cycler_if_moving_min_cycler) + 0.1,
                                                                    abs(distance_to_my_cycler_if_moving_min_cycler)))]
potential_slots[, score_to_my_cycler_if_moving_max_cycler := 2 ^ (20 - ifelse(distance_to_my_cycler_if_moving_max_cycler > 0, abs(distance_to_my_cycler_if_moving_max_cycler) + 0.1,
                                                                    abs(distance_to_my_cycler_if_moving_max_cycler)))]
potential_slots[, join_help := CYCLER_ID %% 2]
aggregate <- potential_slots[, .(sum_min_first = sum(score_to_my_cycler_if_moving_min_cycler),
                                 sum_max_first = sum(score_to_my_cycler_if_moving_max_cycler)), by = TEAM_ID]
#find the cyclers that go to 2nd phase
aggregate[, join_help := ifelse(sum_min_first < sum_max_first, 1, 0)]

#join agg
drop_to_2nd_phase <- potential_slots[aggregate, on = .(join_help, TEAM_ID), CYCLER_ID]


adjusted_first_phase_cyclers <- setdiff(first_phase_cyclers, drop_to_2nd_phase)

} else {
  adjusted_first_phase_cyclers <- first_phase_cyclers
}
second_phase_cyclers <-  setdiff(move_order[, CYCLER_ID], adjusted_first_phase_cyclers)

for (cyc_loop in adjusted_first_phase_cyclers) {
  loop_movement <- move_simul_data[CYCLER_ID == cyc_loop, MOVEMENT]

  cop_game <- move_cycler(cop_game, cyc_loop, loop_movement, FALSE, FALSE, FALSE, FALSE)

}
#there can be now max 1 cycler and max 1 dummy per team per slot
#cop_game[, is_dummy := ifelse(CYCLER_ID < 0, TRUE, ifelse(CYCLER_ID > 0, FALSE, NA))]

#count_teams <- cop_game[CYCLER_ID != 0, .N, by = .(GAME_SLOT_ID, CYCLER_ID, is_dummy)]
#count_teams[, dummy_id := abs(CYCLER_ID)]
#join_team <- STG_CYCLER[count_teams, on = .(CYCLER_ID = dummy_id)]
#now calc the max
#aggr_slots <- join_team[, .N, by = .(TEAM_ID, is_dummy, GAME_SLOT_ID)]

#move my cycler_now
my_move <- move_simul_data[CYCLER_ID == my_first_cycler, MOVEMENT]

cop_game <- move_cycler(cop_game, my_first_cycler, my_move, FALSE,
                                 ignore_block = FALSE, FALSE,  return_numeric_position = FALSE)


# #check if new slot is occupied
# cyclers_in_slot <- cop_game[CYCLER_ID != 0 & GAME_SLOT_ID == my_new_slot, .(CYCLER_ID, SQUARE_ID)]
# cyclers_in_slot[, cycler_fix_dummy := abs(CYCLER_ID)]
# #join team
# cyclers_slot_team <- STG_CYCLER[cyclers_in_slot, on = .(CYCLER_ID = cycler_fix_dummy)][order(-SQUARE_ID)]
# #my positon after each teams first cycler and ahead of second cyclers
# cyclers_slot_team[, team_order := seq_len(.N), by = TEAM_ID]

#dont move me again
adjusted_second_phase <- setdiff(second_phase_cyclers, my_first_cycler)
for (cyc_loop in adjusted_second_phase) {
  loop_movement <- move_simul_data[CYCLER_ID == cyc_loop, MOVEMENT]

  cop_game <- move_cycler(cop_game, cyc_loop, loop_movement, FALSE, FALSE, FALSE, FALSE)



}

#slipstream

cop_game <- apply_slipstream(cop_game)


if (return_pos_vec == TRUE) {

  #new_pos <- cop_game[CYCLER_ID > 0, .(CYCLER_ID, GAME_SLOT_ID)]

  #https://stackoverflow.com/questions/37878620/reorder-rows-in-data-table-in-a-specific-order
  #order the cyclers in the original sorted order that was in the inpt data
#new_pos <- setorder(new_pos[, .r := order(-move_simul_data[, CYCLER_ID])], .r)[, .r := NULL]


  res <- cop_game[order(CYCLER_ID)][CYCLER_ID > 0, GAME_SLOT_ID]
} else {
  res <- cop_game
}
 return(res)
}
  #create dummy cyclers

#
# ssrange <- range_joined_team[, .(MOVEMENT = max(MOVEMENT)), by = .(CYCLER_ID)]
# ssrange
#
# append <- rbind(ssrange[,.(CYCLER_ID, calc_slot = curr_pos)], ssrange[,.(CYCLER_ID, calc_slot = NEW_SLOT)])
#
# join_order <- CYCLER_ORDER[append, on = "CYCLER_ID"]
#
# cycler_id <- 2
#
# my_cycler_slot <- ssrange[CYCLER_ID == cycler_id, NEW_SLOT]
#
# traffic <- join_order[, .N, by = calc_slot]
# traffic[, lanes := 2]
#
# order <- traffic[order(calc_slot)]
#
#
#
# #find first missing space
#
#
#
# game_slots <- game_status[, .(GAME_SLOT_ID = max(GAME_SLOT_ID), LANE_COUNT = .N), by = GAME_SLOT_ID][order(-GAME_SLOT_ID)]
# game_slots[, LANE_COUNT := 2]
# join_slots <- order[game_slots, on = .(calc_slot = GAME_SLOT_ID)]
# first_ms <- join_slots[N > LANE_COUNT , max(calc_slot)]
# relevant_rows <- join_slots[GAME_SLOT_ID <= first_ms]
# relevant_rows[, missing_space := ifelse(is.na(N), LANE_COUNT, LANE_COUNT - N)]
# relevant_rows[, cumsum_space := cumsum(missing_space)]
# relevant_rows
# [calc_slot == my_cycler_slot]
#
# CYCLER_ORDER
