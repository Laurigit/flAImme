#test create_move_order_vec



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


cycler_ids <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)






test_that("moves are done in correct order", {
  random <- sample(cycler_ids, 6)
  correct_answer <- game_status[order(-SQUARE_ID)][CYCLER_ID > 0 & CYCLER_ID %in% random, CYCLER_ID]
  expect_equal(create_move_order_vec(game_status, random), correct_answer)
})

