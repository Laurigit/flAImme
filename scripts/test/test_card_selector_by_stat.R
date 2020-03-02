required_data(c("STG_TRACK", "STG_TRACK_PIECE", "ADM_CYCLER_DECK"))
input_STARTUP_DATA <- data.table(CYCLER_ID = c(1,2,3,4,5,6,7,8,9,10),
                                 PLAYER_ID = c(1,1,2,2,3,3,4,4,5,5),
                                 exhaust = c(0, 0, 0, 0, 0, 0,0,0,0,0),
                                 starting_row =   c(1, 1, 1, 2, 2, 2,3,3,3,4),
                                 starting_lane = c(1,2, 3, 1, 2, 3,1,2,3,1))
track <- 14

game_status <- start_game(used_startup_data,track, STG_TRACK_PIECE, STG_TRACK)
game_status <- set_cycler_position(1, 20, 1,  game_status)
deck_status <- create_decks(input_STARTUP_DATA[, CYCLER_ID], ADM_CYCLER_DECK)
options <- deck_status[row_id %in% c(1, 16, 32)]
zoom(game_status)

library(testthat)
test_that("downhill options", {

  expect_equal(card_selector_by_stat(
    game_status, options, 1, "SMART_MAX",  aim_downhill = FALSE)[, MOVEMENT], 3
              )
})
