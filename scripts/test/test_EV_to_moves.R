ev_table <- data.table(CYCLERS = "1_2", TEAM_ID = 1, MOVES = c("3_3", "3_2", "2_3", "2_2"), EV = c(1.04, 1.03, 1.02, 1.01), M1 = c(3, 3, 2, 2), M2 = c(3, 2, 3, 2),
           C1 = c(1), C2 = c(2), draw_odds_C1 = c(0.1, 0.1, 0.9, 0.9), draw_odds_C2 = c(0.2, 0.8, 0.2, 0.8),
           TURNS_TO_FINISH = c(1, 1, 1, 10))


deck_status <- data.table(CYCLER_ID = c(1, 1, 2, 2), CARD_ID = c(3, 2, 3, 2), Zone = c("Hand"), MOVEMENT = c(3, 2, 3, 2), row_id = 1:4)
turn_start_deck <- data.table(CYCLER_ID = c(1, 1, 2, 2), CARD_ID = c(3, 2, 3, 2), Zone = c("Deck"), MOVEMENT = c(3, 2, 3, 2), row_id = 1:4)
test_rest <- EV_to_moves(ev_table, deck_status, turn_start_deck)
