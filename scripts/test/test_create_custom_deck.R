

id_vec <- c(1, 2, 3, 4, 5)
zone_vec <- c(1, 2, 3, 4, 1)
required_data("ADM_CYCLER_DECK")
test_that("create correct deck", {

  test_res <- data.table(CYCLER_ID = 1, CARD_ID = c(1, 2, 3, 4, 5),
                         Zone = c("Deck", "Hand", "Recycled", "Removed", "Deck"),
                         MOVEMENT = c(2, 2, 3, 4, 5),
                         row_id = c(1, 2, 3, 4, 5))
expect_equal( create_custom_deck(1, id_vec, zone_vec, ADM_CYCLER_DECK), test_res)
})
