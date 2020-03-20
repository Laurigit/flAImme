#test play card



test_that("exhaustion is played if multiple options", {
  con <- connDB(con, "flaimme")

  required_data(c("STG_TRACK", "STG_TRACK_PIECE", "ADM_CYCLER_DECK", "STG_TEAM"))

  track <- 1


  #game_status <- set_cycler_position(1, 64, 1,  game_status)

  deck_status <- create_decks(c(1, 2, 3, 4, 5, 6), ADM_CYCLER_DECK)
  #deck_status <- draw_cards(1, deck_status, 4)
  deck_status[CYCLER_ID == 2]
  deck_status[row_id %in% c(46,55,64,73), Zone := "Hand"]
  deck_status[CYCLER_ID == 2]
  deck_status <- add_exhaustion(2, deck_status, "Hand")
  deck_status <- play_card(2, NULL, deck_status, 0,0, FALSE, NULL, MOVEMENT_PLAYED = 2, force = FALSE, copy = FALSE)
  expect_equal(deck_status[CYCLER_ID == 2 & Zone == "Removed", CARD_ID], 1)

})

test_that("force plays from deck", {
  con <- connDB(con, "flaimme")

  required_data(c("STG_TRACK", "STG_TRACK_PIECE", "ADM_CYCLER_DECK", "STG_TEAM"))

  track <- 1



  deck_status <- create_decks(c(1, 2, 3, 4, 5, 6), ADM_CYCLER_DECK)
  #deck_status <- draw_cards(1, deck_status, 4)
  deck_status[CYCLER_ID == 2]

  deck_status <- add_exhaustion(2, deck_status, "Recycle")
  deck_status <- play_card(2, NULL, deck_status, 0,0, FALSE, NULL, MOVEMENT_PLAYED = 2, force = TRUE, copy = FALSE)
  expect_equal(deck_status[CYCLER_ID == 2 & Zone == "Removed", CARD_ID], 1)

})
