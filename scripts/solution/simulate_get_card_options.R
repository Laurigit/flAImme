#simulate_playing_cards

simulate_get_card_options <- function(deck_status, cycler_id) {
  #if cards in deck >= 4, then use them first
  cards_in_deck <- deck_status[CYCLER_ID == cycler_id & Zone == "Deck"]
  count_cards <- nrow(cards_in_deck)
  #how many full 4 card hands left in deck
  full_hands <- floor(count_cards / 4)

  turn <- 0
  turn_total <- NULL
  if (full_hands > 0) {
  for(hand_loop in full_hands) {
    turn <- turn + 1
    turn_cards <- cbind(deck_status[CYCLER_ID == cycler_id & Zone == "Deck", .(row_id, MOVEMENT)], turn_simulate = turn)
    turn_total <- rbind(turn_total, turn_cards)
  }
  }
  #rest of turns after shuffle
  final_cards <- cbind(deck_status[CYCLER_ID == cycler_id & Zone != "Removed", .(row_id, MOVEMENT)], turn_simulate = 0)
  turn_total <- rbind(turn_total, final_cards)
  #if not, allow every card and note the odds
  return(turn_total)
}
