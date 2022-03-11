required_data(c("STG_TRACK", "STG_TRACK_PIECE", "ADM_CYCLER_DECK"))
input_STARTUP_DATA <- data.table(CYCLER_ID = c(1,2,3,4,5,6,7,8,9,10),
                                 PLAYER_ID = c(1,1,2,2,3,3,4,4,5,5),
                                 exhaust = c(0, 0, 0, 0, 0, 0,0,0,0,0),
                                 starting_row =   c(1, 1, 1, 2, 2, 2,3,3,3,4),
                                 starting_lane = c(1,2, 3, 1, 2, 3,1,2,3,1))
track <- 1

game_status <- start_game(c(1),track, STG_TRACK_PIECE, STG_TRACK)
game_status <- set_cycler_position(1, 64, 1,  game_status)
deck_status <- create_decks(input_STARTUP_DATA[, CYCLER_ID], ADM_CYCLER_DECK)
pre_track <- precalc_track(game_status)
options <- "77766655544433322"
options <- "76555553333"
zoom(game_status, 17)

library(testthat)
optimal_moves_to_finish(options,  1, pre_track$aggr_to_slots, draw_odds_raw_data  = "")
dodds <- data.table(Turn_to_Draw = c(1, 1, 2), MOVEMENT = c(2, 5, 7), prob = 0)
options <- create_custom_deck(1, c(5, 2, 7, 3), c(1, 1, 1, 1))
otions_string <- options[, paste0(MOVEMENT, collapse = "")]
optimal_moves_to_finish(options, 6, pre_track$aggr_to_slots, draw_odds_raw_data = "11112222;23572357")
test_that("downhill options", {

  expect_equal(optimal_moves_to_finish(options,  64, pre_track$aggr_to_slots, draw_odds_raw_data  = "")[, TURNS_TO_FINISH], 2)
 expect_equal(optimal_moves_to_finish(options,  79, pre_track$aggr_to_slots, draw_odds_raw_data  = "")[, TURNS_TO_FINISH], 1)

})

test_that("draw odds", {
  game_status <- start_game(used_startup_data,6, STG_TRACK_PIECE, STG_TRACK)
  game_status <- set_cycler_position(1, 62, 1,  game_status)
  zoom(game_status, 25)
  options <- create_custom_deck(1, c(5, 2, 7, 3), c(1, 1, 1, 1))
  otions_string <- options[, paste0(MOVEMENT, collapse = "")]
  pre_track <- precalc_track(game_status)

  expect_equal(optimal_moves_to_finish(otions_string,  62, pre_track, use_draw_odds = dodds)[, TURNS_TO_FINISH], 5)
  expect_equal(optimal_moves_to_finish(otions_string,  61, pre_track, use_draw_odds = "")[, TURNS_TO_FINISH], 4)
})

test_that("full track", {
  game_status <- start_game(c(1),12, STG_TRACK_PIECE, STG_TRACK)

  options <- "777666555444333"
  pre_track <- precalc_track(game_status)
  expect_equal(optimal_moves_to_finish(options,  1, pre_track, use_draw_odds = "")[, TURNS_TO_FINISH], 15)
  options <- "999555444333222"
  expect_equal(optimal_moves_to_finish(options,  1, pre_track, use_draw_odds = "")[, TURNS_TO_FINISH], 18)

  })
