# required_data(c("STG_CYCLER", "STG_TRACK"))
# input_STARTUP_DATA <- data.table(CYCLER_ID = c(1,2,3,4,5,6),
#                                  PLAYER_ID = c(1,1,2,2,3,3),
#                                  exhaust = c(0, 0, 0, 0, 0, 0),
#                                  starting_row =   c(3, 2, 3, 1, 2, 1),
#                                   starting_lane = c(2, 1, 1, 1, 2, 2))
#  track <- 2
required_data(c("STG_TRACK", "SRC_TRACK", "SRC_TRACK_PIECE", "STG_TRACK_PIECE"), force_update =TRUE)
total_winner <- NULL
for (game_id in 1:10000) {
winner_state <- data.table(TURN_ID = numeric(), CYCLER_ID = numeric(), POSITION = numeric(), GAME_ID = numeric(),
                           row_over_finish = numeric(), finish_square = numeric())

game_status <- start_game(input_STARTUP_DATA,3, STG_TRACK_PIECE, STG_TRACK)
#add_mountain_info
game_status <- slots_out_of_mountains(game_status)

#initial decks
deck_status <- create_decks(input_STARTUP_DATA[, CYCLER_ID], ADM_CYCLER_DECK)


cyclers <- input_STARTUP_DATA[, CYCLER_ID]

#draw phase
for(turn_id in 1:25) {
#print(turn_id)

for(loop in cyclers) {
  deck_status <- draw_cards(loop, deck_status)
}

#move phase
#print("KORTTVALINNAT")

pelatut_kortit <- deck_status[sample(nrow(deck_status))][Zone == "Hand", .SD[1:1], .(CYCLER_ID)][, .(CYCLER_ID, CARD_ID, MOVEMENT)][CYCLER_ID %in% cyclers]
pelatut_kortit[CYCLER_ID == 6, CARD_ID := card_selector_by_stat(game_status, deck_status[CYCLER_ID == 6 & Zone == "Hand"], 6, "SMART_MAX",  aim_downhill = TRUE)[, MOVEMENT]]
pelatut_kortit[CYCLER_ID == 6, MOVEMENT := CARD_ID]
pelatut_kortit[CYCLER_ID == 2, CARD_ID := card_selector_by_stat(game_status, deck_status[CYCLER_ID == 2 & Zone == "Hand"], 2, "SMART_MAX",  aim_downhill = TRUE)[, MOVEMENT]]
pelatut_kortit[CYCLER_ID == 2, MOVEMENT := CARD_ID]
pelatut_kortit[CYCLER_ID == 3, CARD_ID := card_selector_by_stat(game_status, deck_status[CYCLER_ID == 3 & Zone == "Hand"], 3, "SMART_MAX",  aim_downhill = TRUE)[, MOVEMENT]]
pelatut_kortit[CYCLER_ID == 3, MOVEMENT := CARD_ID]
pelatut_kortit[CYCLER_ID == 4, CARD_ID := card_selector_by_stat(game_status, deck_status[CYCLER_ID == 4 & Zone == "Hand"], 4, "SMART_MAX",  aim_downhill = TRUE)[, MOVEMENT]]
pelatut_kortit[CYCLER_ID == 4, MOVEMENT := CARD_ID]
pelatut_kortit[CYCLER_ID == 5, CARD_ID := card_selector_by_stat(game_status, deck_status[CYCLER_ID == 5 & Zone == "Hand"], 5, "SMART_MAX",  aim_downhill = TRUE)[, MOVEMENT]]
pelatut_kortit[CYCLER_ID == 5, MOVEMENT := CARD_ID]
#pelatut_kortit[CYCLER_ID == 6, CARD_ID := card_selector_by_stat(game_status, deck_status[CYCLER_ID == 6 & Zone == "Hand"], 6, "MAX",  aim_downhill = TRUE)[, MOVEMENT]]
#pelatut_kortit[CYCLER_ID == 6, MOVEMENT := CARD_ID]
#action_data <- data.table(CYCLER_ID = c(1,2,3,4,5,6), MOVEMENT = c(2,3,2,5,4,3), move_order = c(1,2,3,4,5,6), TEAM_ID = c(1,1,2,2,3,3))

if (nrow(winner_state[CYCLER_ID == 1]) != 1) {
if (turn_id <= 20) {
action_data3 <- pelatut_kortit[,. (CYCLER_ID, MOVEMENT)]
CYCLER_ORDER <- game_status[CYCLER_ID > 0][order(-SQUARE_ID)][, .(CYCLER_ID, move_order = seq_len(.N))]
action_data2 <- CYCLER_ORDER[action_data3, on = "CYCLER_ID"]
ss_team <- STG_CYCLER[, .(CYCLER_ID, TEAM_ID)]
action_data1 <- ss_team[action_data2, on = "CYCLER_ID"]
#cycler 1 hand
my_hand <- deck_status[CYCLER_ID == 1 & Zone == "Hand"]
best_move <- NULL
tot_analysis <- data.table(Setting = "")
for (best_loop in my_hand[, .N, by = MOVEMENT][, MOVEMENT]) {
  action_data1[CYCLER_ID == 1, MOVEMENT := best_loop]
pos_score <- suppressWarnings(score_position(game_status, deck_status, action_data1, ADM_AI_CONF))[!is.na(Score)]
my_score <- pos_score[CYCLER_ID == 1, sum(Result)]
#print(paste0("MY ", my_score))
my_analysis <- pos_score[CYCLER_ID == 1, sum(Result), by = Setting]
tot_analysis <- tot_analysis[my_analysis, on = "Setting"]
#print( pos_score[CYCLER_ID == 1])
#kovin_vihu <- AI(cycler_id = 1, game_status, deck_status)
enemy_score <- pos_score[CYCLER_ID != 1, sum(Result), by = CYCLER_ID][, mean(V1)]
#print(paste0("ENEM ", enemy_score))
#print( pos_score[CYCLER_ID != 1, sum(Result), by = Setting])
row_data <- data.table(tot_score = my_score - 0, MOVEMENT = best_loop)
best_move <- rbind(row_data, best_move)
#print(pos_score[CYCLER_ID == 1])
}
selected_move <- best_move[ , .SD[which.max(tot_score)]][, MOVEMENT]
#print(zoom(game_status))
#print(tot_analysis)
#print(selected_move)
#browser()
pelatut_kortit[CYCLER_ID == 1, ':=' (MOVEMENT = selected_move, CARD_ID = selected_move) ]
} else {
  pelatut_kortit[CYCLER_ID == 1, CARD_ID := card_selector_by_stat(game_status, deck_status[CYCLER_ID == 1 & Zone == "Hand"], 1, "SMART_MAX",  aim_downhill = FALSE)[, MOVEMENT]]
  pelatut_kortit[CYCLER_ID == 1, MOVEMENT := CARD_ID]

}
}
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
#print(zoom(game_status))
#mukana_olevat_viela <- game_status[CYCLER_ID > 0, CYCLER_ID]

# }
}
total_winner <- rbind(total_winner, winner_state)
print(total_winner[, mean(POSITION), by = CYCLER_ID][order(V1)])
}
