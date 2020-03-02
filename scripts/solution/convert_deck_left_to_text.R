convert_deck_left_to_text <- function(deck_status, cycler_id) {

  deck_left <- deck_status[order(-MOVEMENT)][CYCLER_ID == cycler_id & Zone != "Removed", paste0(MOVEMENT, collapse = "")]
  return(deck_left)
}
