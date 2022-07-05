convert_deck_left_to_text <- function(deck_status, cycler_id, played_card = FALSE, trunc = FALSE,
                                      max_extra_exhaust = 1) {

if (played_card == FALSE) {

  copy_deck <- copy(deck_status)
} else {
  copy_deck <- copy(deck_status)
  remove_movement <- copy_deck[CYCLER_ID == cycler_id & MOVEMENT == played_card & Zone != "Removed"]
  remove_row_id <-suppressWarnings(remove_movement[CARD_ID ==  min(CARD_ID), min(row_id)])
  copy_deck <- copy_deck[row_id != remove_row_id]

}
  #remove extra 2 from the deck

  if (trunc == TRUE) {
    sorted_deck <- copy_deck[order(CYCLER_ID, MOVEMENT)]
    sorted_deck[, nth_card := seq_len(.N), by = .(CYCLER_ID, MOVEMENT)]
    filtered_deck <- sorted_deck[nth_card <= 3]
  } else {
    filtered_deck <- copy_deck
  }
  count_exh <- filtered_deck[CARD_ID == 1 & cycler_id == CYCLER_ID & Zone != "Removed"]
  cut_cards <- nrow(count_exh) - max_extra_exhaust
  total_cards_included <- filtered_deck[cycler_id == CYCLER_ID & Zone != "Removed", .N] - cut_cards
  deck_left <- str_sub(filtered_deck[order(-MOVEMENT)][CYCLER_ID == cycler_id & Zone != "Removed", paste0(MOVEMENT, collapse = "")],
                                                       1, total_cards_included)
  return(deck_left)
}
