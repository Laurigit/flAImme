
con <- connDB(con, dbname_input = "flaimme")
rm("ADM_OPTIMAL_MOVES")
required_data(c("STG_TRACK", "STG_TRACK_PIECE", "ADM_CYCLER_DECK", "ADM_OPTIMAL_MOVES"))

track <- 12

game_status <- start_game(c(1, 3, 5, 2, 4, 6, 7, 8, 9, 10, 11, 12),track, STG_TRACK_PIECE, STG_TRACK)
#game_status <- set_cycler_position(1, 64, 1,  game_status)
deck_status <- create_decks(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), ADM_CYCLER_DECK)
pre_track <- precalc_track(game_status)
ctM_res <- cyclers_turns_MOVEMEMENT_combs(con, ADM_OPTIMAL_MOVES, game_status, deck_status, pre_track)
ctM_data <- ctM_res$ctM_data

range <- calc_move_range(game_status, deck_status, ctM_data, STG_TEAM)
range_ss <- range[, .(MOVEMENT = max(MOVEMENT)), by = .(CYCLER_ID, curr_pos)]
range_ss <- range[c(1, 6, 11, 17, 23, 27, 34, 38, 44, 50, 55, 60), .(MOVEMENT, CYCLER_ID, curr_pos, TEAM_ID)]
zoom(game_status)


test_res <- data.table(GAME_SLOT_ID = c(4, 5, 6, 7, 8),
                       PIECE_ATTRIBUTE = c("N", "N", "N", "N", "N"),
                       FINISH = c(0, 0, 0, 0, 0),
                       CYCLERS = c("12-10-0",
                                  "0-0-11",
                                  "0-0-0",
                                  "9-8-7",
                                  "0-0-0"))
test_that("opponents will block if possible",{


  game_status_test  <- phaseless_simulation(game_status, 12,
                                            11,
                                            6, STG_CYCLER, range_ss)

  expect_equal(zoom(game_status_test)[1:5], test_res)
})




track <- 12

game_status <- start_game(c(1, 3, 5, 2, 4, 6, 7, 8, 9, 10, 11, 12),track, STG_TRACK_PIECE, STG_TRACK)
#game_status <- set_cycler_position(1, 64, 1,  game_status)
deck_status <- create_decks(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), ADM_CYCLER_DECK)
pre_track <- precalc_track(game_status)
ctM_res <- cyclers_turns_MOVEMEMENT_combs(con, ADM_OPTIMAL_MOVES, game_status, deck_status, pre_track)
ctM_data <- ctM_res$ctM_data

range <- calc_move_range(game_status, deck_status, ctM_data, STG_TEAM)
range_ss <- range[, .(MOVEMENT = max(MOVEMENT)), by = .(CYCLER_ID, curr_pos)]
range_ss <- range[c(1, 9, 11, 19, 23, 29, 34, 38, 44, 50, 53, 57), .(MOVEMENT, CYCLER_ID, curr_pos, TEAM_ID)]
zoom(game_status)


test_res <- data.table(GAME_SLOT_ID = c(4, 5, 6, 7, 8),
                       PIECE_ATTRIBUTE = c("N", "N", "N", "N", "N"),
                       FINISH = c(0, 0, 0, 0, 0),
                       CYCLERS = c("0-0-10",
                                   "0-11-8",
                                   "12-6-4",
                                   "2-9-7",
                                   "0-0-0"))

test_that("opponents will also move to block",{

  zoom(game_status)
  game_status_test  <- phaseless_simulation(game_status, 12,
                                            11,
                                            6, STG_CYCLER, range_ss)
  zoom(game_status_test)

  expect_equal(zoom(game_status_test)[1:5], test_res)
})


