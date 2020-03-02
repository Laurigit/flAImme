

required_data(c("STG_TRACK", "STG_TRACK_PIECE", "ADM_CYCLER_DECK", "STG_TEAM"))
input_STARTUP_DATA <- data.table(CYCLER_ID = c(1,2,3,4,5,6,7,8,9,10),
                                 PLAYER_ID = c(1,1,2,2,3,3,4,4,5,5),
                                 exhaust = c(0, 0, 0, 0, 0, 0,0,0,0,0),
                                 starting_row =   c(1, 1, 1, 2, 2, 2,3,3,3,4),
                                 starting_lane = c(1,2, 3, 1, 2, 3,1,2,3,1))
track <- 1

game_status <- start_game(c(1, 2, 3, 4, 5, 6),track, STG_TRACK_PIECE, STG_TRACK)
#game_status <- set_cycler_position(1, 64, 1,  game_status)
deck_status <- create_decks(c(1, 2, 3, 4, 5, 6), ADM_CYCLER_DECK)
pre_track <- precalc_track(game_status)
#options <- create_custom_deck(1, c(6, 2, 2), c(1, 1, 1))
zoom(game_status, 17)
ctM_res <- cyclers_turns_MOVEMEMENT_combs(con, ADM_OPTIMAL_MOVES, game_status, deck_status, pre_track)
ctM_data <- ctM_res$ctM_data
library(testthat)
test_that("team_odds_ add to 1", {
 res <-  calc_move_range(game_status, deck_status, ctM_data, STG_TEAM)
 expect_equal(res[, .(sum_odds = sum(splitted_odds)), by = TEAM_ID][, sum_odds] == 1, c(TRUE, TRUE, TRUE))
})
