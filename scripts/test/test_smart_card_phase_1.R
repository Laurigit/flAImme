con <- connDB(con, dbname_input = "flaimme")
rm("ADM_OPTIMAL_MOVES")
required_data(c("STG_TRACK", "STG_TRACK_PIECE", "ADM_CYCLER_DECK", "ADM_OPTIMAL_MOVES"))
required_data("ADM_AI_CONF")
#track <- 2
cyclers <- game_status[CYCLER_ID > 0, CYCLER_ID]
#game_status <- start_game(cyclers,track, STG_TRACK_PIECE, STG_TRACK)
#game_status <- set_cycler_position(1, 64, 1,  game_status)
#deck_status <- create_decks(cyclers, ADM_CYCLER_DECK)



#everybody draws
# for(loop in cyclers) {
#   deck_status <- draw_cards(loop, deck_status)
#   # print(list(deck_status[CYCLER_ID == loop & Zone == "Hand", CARD_ID]))
#
# }


pre_track <- precalc_track(game_status)
ctM_res <- cyclers_turns_MOVEMEMENT_combs(con, ADM_OPTIMAL_MOVES, game_status, deck_status, pre_track)
smart_cyclers <- c(5, 6)
first_cycler <- 5
second_cycler <- 6
range <- calc_move_range(game_status, deck_status, ctM_data, STG_TEAM)
team_id <- 3
select_smart_card_phase_1(game_status, deck_status, smart_cyclers, pre_track, ctM_res, range, first_cycler, second_cycler, team_id)
