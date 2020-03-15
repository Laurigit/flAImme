

rm("ADM_OPTIMAL_MOVES")
required_data(c("STG_TRACK", "STG_TRACK_PIECE", "ADM_CYCLER_DECK", "ADM_OPTIMAL_MOVES"))
cycler_id <- 1
track <- 12

game_status <- start_game(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),track, STG_TRACK_PIECE, STG_TRACK)
#game_status <- set_cycler_position(1, 64, 1,  game_status)
deck_status <- create_decks(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), ADM_CYCLER_DECK)
zoom(game_status)
game_status <- set_cycler_position(4, 8, lane = NULL, game_status)
game_status <- set_cycler_position(5, 8, lane = NULL, game_status)
game_status <- set_cycler_position(6, 8, lane = NULL, game_status)
game_status <- set_cycler_position(8, 7, lane = NULL, game_status)
game_status <- set_cycler_position(9, 7, lane = NULL, game_status)
game_status <- set_cycler_position(10, 7, lane = NULL, game_status)
zoom(game_status)
#game_status <- move_cycler(game_status, cycler_id, 3, FALSE, ignore_block = TRUE, FALSE, FALSE)
copy_status <- copy(game_status)

test_that("move_cyclcer ignore block pushes correctly full lanes",{

  test_res <- move_cycler(copy_status, cycler_id, 3, FALSE, ignore_block = TRUE, FALSE, FALSE)
  expect_equal(zoom(test_res)[5, CYCLERS],  "0-0-10")

})

zoom(copy_status)
test_that("move_cyclcer does not push when it collides with blocked lane",{

  test_res <- move_cycler(copy_status, cycler_id = 3, 3,
                             slipstream = FALSE, ignore_block = FALSE,
                             ignore_end_of_track = FALSE, FALSE)
  zoom(test_res)
  expect_equal(zoom(test_res)[5, CYCLERS],  "0-0-10")

})
zoom(game_status)


- played_cards_data[TURN_ID == turn_id & loop_move == CYCLER_ID & PHASE == phase_loop]

for(loop_move in in_game_cyclers) {

  row_data <- played_cards_data[TURN_ID == turn_id & loop_move == CYCLER_ID & PHASE == phase_loop]


  game_status <- move_cycler(game_status, loop_move, movement = row_data[, MOVEMENT])


  #record moves
  row_res <- range_joined_team[CYCLER_ID == row_data[, CYCLER_ID] & MOVEMENT ==  row_data[, MOVEMENT]]
  move_range_data_actual_moves <- rbind(row_res, move_range_data_actual_moves)

  print(zoom(game_status))
}
