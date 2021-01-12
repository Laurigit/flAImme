#test_calculate_mixed_strategy
con <- connDB(con, dbname_input = "flaimme")
required_data(c("STG_TRACK_PIECE", "STG_TRACK", "ADM_CYCLER_DECK", "ADM_OPTIMAL_MOVES", "STG_TEAM", "ADM_AI_CONF"))
track <- 2
cyclers <- c(4,1,3,6,2,5)
game_status <- start_game(cyclers,track, STG_TRACK_PIECE, STG_TRACK)
ijk <- ijk_map(track, STG_TRACK_PIECE, STG_TRACK)
deck_status <- create_decks(cyclers, ADM_CYCLER_DECK)
ctM_res <- cyclers_turns_MOVEMEMENT_combs(con, ADM_OPTIMAL_MOVES, game_status, deck_status, pre_track)
#calculate_mixed_strategy


test_that("total probability = 3", {
  res <- calculate_mixed_strategy(game_status, deck_status, ijk, ADM_AI_CONF, ADM_OPTIMAL_MOVES, ctM_data, STG_TEAM)
  p_tot <- res[, sum(PROB_PRODUCT)]
  expect_equal(p_tot, 3)

})

