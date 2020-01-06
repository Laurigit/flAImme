# required_data("STG_CYCLER")
# input_STARTUP_DATA <- data.table(CYCLER_ID = c(1,2,3,4,5,6),
#                                  PLAYER_ID = c(1,1,2,2,3,3),
#                                  exhaust = c(0, 0, 0, 0, 0, 0),
#                                  starting_row =  c(1,2,1, 3,2,3),
#                                   starting_lane = c(1, 1, 2, 1, 2, 2))
#  track <- 2

winner_state <- NULL

game_status <- start_game(input_STARTUP_DATA, 2, STG_TRACK_PIECE, STG_TRACK)
#initial decks
deck_status <- create_decks(input_STARTUP_DATA[, CYCLER_ID], ADM_CYCLER_DECK)


cyclers <- input_STARTUP_DATA[, CYCLER_ID]
game_id <- 1
#draw phase
for(turn_id in 1:20) {
#print(turn_id)

for(loop in cyclers) {
  deck_status <- draw_cards(loop, deck_status)
}

#move phase

pelatut_kortit <- deck_status[sample(nrow(deck_status))][Zone == "Hand", .SD[1:1], .(CYCLER_ID)][, .(CYCLER_ID, CARD_ID, MOVEMENT)]
pelatut_kortit[CYCLER_ID == 1, CARD_ID := card_selector_by_stat(game_status, deck_status[CYCLER_ID == 1 & Zone == "Hand"], 1, "SMART_MAX",  aim_downhill = TRUE)[, MOVEMENT]]
pelatut_kortit[CYCLER_ID == 1, MOVEMENT := CARD_ID]
pelatut_kortit[CYCLER_ID == 2, CARD_ID := card_selector_by_stat(game_status, deck_status[CYCLER_ID == 2 & Zone == "Hand"], 2, "SMART_MAX",  aim_downhill = TRUE)[, MOVEMENT]]
pelatut_kortit[CYCLER_ID == 2, MOVEMENT := CARD_ID]
pelatut_kortit[CYCLER_ID == 3, CARD_ID := card_selector_by_stat(game_status, deck_status[CYCLER_ID == 3 & Zone == "Hand"], 3, "SMART_MAX",  aim_downhill = TRUE)[, MOVEMENT]]
pelatut_kortit[CYCLER_ID == 3, MOVEMENT := CARD_ID]
pelatut_kortit[CYCLER_ID == 4, CARD_ID := card_selector_by_stat(game_status, deck_status[CYCLER_ID == 4 & Zone == "Hand"], 4, "SMART_MAX",  aim_downhill = FALSE)[, MOVEMENT]]
pelatut_kortit[CYCLER_ID == 4, MOVEMENT := CARD_ID]
pelatut_kortit[CYCLER_ID == 5, CARD_ID := card_selector_by_stat(game_status, deck_status[CYCLER_ID == 5 & Zone == "Hand"], 5, "MAX",  aim_downhill = FALSE)[, MOVEMENT]]
pelatut_kortit[CYCLER_ID == 5, MOVEMENT := CARD_ID]
pelatut_kortit[CYCLER_ID == 6, CARD_ID := card_selector_by_stat(game_status, deck_status[CYCLER_ID == 6 & Zone == "Hand"], 6, "MAX",  aim_downhill = TRUE)[, MOVEMENT]]
pelatut_kortit[CYCLER_ID == 6, MOVEMENT := CARD_ID]

#
#print("Start moving")
for(loop_move in cyclers) {
  row_data <- pelatut_kortit[loop_move == CYCLER_ID]

  deck_status <- play_card(cycler_id = loop_move,
                           card_id = row_data[, CARD_ID],
                           current_decks = deck_status, 1, 1, FALSE)
  game_status <- move_cycler(game_status, loop_move, movement = row_data[, MOVEMENT])

}

game_status <- apply_slipstream(game_status)
deck_status <- apply_exhaustion(deck_status, game_status)
winner_state <- check_winner(game_status, winner_state, turn_id, game_id)
cyclers <- setdiff(cyclers, winner_state[, CYCLER_ID])
game_status <- clear_finishers(game_status,winner_state[, CYCLER_ID])
#game_status[CYCLER_ID > 0][order(-GAME_SLOT_ID)]
#deck_status[, .N, by = .(CYCLER_ID, Zone)][order(Zone, CYCLER_ID)]
}
winner_state
winner_state
