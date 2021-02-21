convert_deck_left_to_text <- function(deck_status, cycler_id, played_card = FALSE) {

if (played_card == FALSE) {
  copy_deck <- copy(deck_status)
} else {
  copy_deck <- copy(deck_status)
  remove_movement <- copy_deck[CYCLER_ID == cycler_id & MOVEMENT == played_card & Zone != "Removed"]
  remove_row_id <- remove_movement[CARD_ID ==  min(CARD_ID), min(row_id)]
  copy_deck <- copy_deck[row_id != remove_row_id]

}
  deck_left <- copy_deck[order(-MOVEMENT)][CYCLER_ID == cycler_id & Zone != "Removed", paste0(MOVEMENT, collapse = "")]
  return(deck_left)
}
