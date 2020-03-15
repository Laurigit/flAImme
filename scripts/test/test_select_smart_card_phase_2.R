






track <- 2
cyclers <- c(1,2,3,4,5,6)
game_status <- start_game(cyclers,track, STG_TRACK_PIECE, STG_TRACK)
deck_copied <- copy(deck_status)



range <- calc_move_range(game_status, deck_status, ctM_data, STG_TEAM)
pre_track <- precalc_track(game_status)

#I decide to move 5,

moevs <- range[row_id %in% c(1, 7, 21), .(MOVEMENT, CYCLER_ID)]
#move cyclers
move_range_data <- NULL

for (loop_move in 1:nrow(moevs)) {
game_status<-  move_cycler(game_status, moevs[loop_move, CYCLER_ID], moevs[loop_move, MOVEMENT], slipstream = FALSE,
              ignore_block = FALSE,
              )
row_res <- range[CYCLER_ID == moevs[loop_move, CYCLER_ID] & MOVEMENT ==  moevs[loop_move, MOVEMENT]]
move_range_data <- rbind(row_res, move_range_data)

}
zoom(game_status)



phase_two_cyclers <- c(3,4,6)
p2_score_outout <- phase2_slot_score(game_status, phase_two_cyclers, deck_copied, range, pre_track)

range2 <- calc_move_range_phase_2(game_status, deck_status, ctM_data, p2_score_outout, STG_CYCLER )

range2_filter <- range2[prio_group < 90]
#append already_moved
new_and_old <- rbind(range2_filter, move_range_data[, .(CYCLER_ID, TEAM_ID, prio_group, MOVEMENT, odds = 1, TURNS_TO_FINISH,
                                                      actual_movement, new_slot_after_moving = new_slot, DECK_LEFT,
                                                      TRACK_LEFT, curr_pos = new_slot, splitted_odds)])

setnames(new_and_old, "new_slot_after_moving", "new_slot")


required_data("ADM_OPTIMAL_MOVES")

ers <- select_smart_card_phase_2(game_status, deck_status, smart_cyclers,
                                      pre_track, ctM_res, new_and_old, first_cycler, second_cycler, team_id, phase_two_cyclers)
