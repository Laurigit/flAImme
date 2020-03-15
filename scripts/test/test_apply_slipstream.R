#test apply slipstream


con <- connDB(con, dbname_input = "flaimme")
rm("ADM_OPTIMAL_MOVES")
required_data(c("STG_TRACK", "STG_TRACK_PIECE", "ADM_CYCLER_DECK", "ADM_OPTIMAL_MOVES"))
required_data("ADM_AI_CONF")
track <- 12
cyclers <- c(1,2,3,4,5,6)
game_status <- start_game(cyclers,track, STG_TRACK_PIECE, STG_TRACK)
#game_status <- set_cycler_position(1, 64, 1,  game_status)
deck_status <- create_decks(cyclers, ADM_CYCLER_DECK)
zoom(game_status)

game_status <- set_cycler_position(1, 12, 1, game_status)
game_status <- set_cycler_position(2, 12, 2, game_status)
game_status <- set_cycler_position(3, 12, 3, game_status)
game_status <- set_cycler_position(4, 14, 1, game_status)

zoom(game_status)
game_status <- apply_slipstream(game_status)
res_zoom <- zoom(game_status)[9:11]

result_compare <- data.table(GAME_SLOT_ID = c(12, 13, 14),
                             PIECE_ATTRIBUTE = c("N", "N", "N"),
                             FINISH = c(0, 0, 0),
                             CYCLERS = c("0-0-3",
                                         "2-1",
                                         "0-4"))

test_that("slipstream works from 3 lanes to 2", {
  expect_equal(res_zoom, result_compare)

})
