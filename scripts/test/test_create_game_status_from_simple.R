#test_create_game_status_from_simple
required_data(c("STG_TRACK", "STG_TRACK_PIECE"))
simple_gs_tes <- data.table(CYCLER_ID = c(1, 2, 3), SQUARE_ID = c(20, 25, 27))
track_id <- 1

test_that("CYCLers are set to correct position",{
  new_status <- create_game_status_from_simple(simple_gs_tes, track_id, STG_TRACK, STG_TRACK_PIECE)
  tst_row <- new_status[CYCLER_ID == 3, GAME_SLOT_ID]
  expect_equal(tst_row, 14)
})

