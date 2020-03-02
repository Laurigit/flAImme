





test_that("track is created correctly", {
  required_data("STG_TRACK_PIECE", "STG_TRACK")
  res <- start_game(c(1,2,3,4), 1, STG_TRACK_PIECE, STG_TRACK, force_lanes = NULL)
  expect_equal(res[START == 1, CYCLER_ID], c(2, 1))
})
