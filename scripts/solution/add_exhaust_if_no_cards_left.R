#add_exhaust_if_no_cards_left. Zone is Recycle
add_exhaust_if_no_cards_left <- function(deck_status, cyclers_left) {
  cyclers_with_cards_left <- deck_status[Zone != "Removed" & CYCLER_ID %in% cyclers_left  , .N, by = CYCLER_ID][, CYCLER_ID]
  cycs_without_cards <- setdiff(cyclers_left, cyclers_with_cards_left)
  max_row_id <- deck_status[, max(row_id)]
  loopperi <- 0
  for (loop_cycler in cycs_without_cards) {
    loopperi <- loopperi + 1
    new_row <- data.table(CYCLER_ID = loop_cycler, CARD_ID = 1, Zone = "Deck", MOVEMENT = 2, row_id = max_row_id + loopperi)
    deck_status <-  rbind(deck_status, new_row)
  }


  return(deck_status)
}
