#draw cards
#current_decks_input <- create_decks(input_STARTUP_DATA[, CYCLER_ID], ADM_CYCLER_DECK)

#test <- draw_cards(1, current_decks_input)

draw_cards <- function(cycler_id, current_decks_input, how_many_cards = 4, rest_of_deck = FALSE) {

  #1 check if cards left in deck
  #for_debug <- current_decks_input#[cycler_id == CYCLER_ID]
  curr_cards <- current_decks_input
  curr_deck_cards <- curr_cards[cycler_id == CYCLER_ID & Zone == "Deck"]

  #check if enough cards
  card_count <-  nrow(curr_deck_cards)
  recycle_count <-  curr_cards[cycler_id == CYCLER_ID & Zone == "Recycle", .N]

  #rest of deck check
  if (rest_of_deck == TRUE) {
    #draw rest of or at least the input value of how_many_cards
    how_many_cards <- max(card_count, how_many_cards)

  }


  #if no cards, shuffle
  if (card_count == 0) {

    curr_cards[cycler_id == CYCLER_ID & Zone == "Recycle", Zone := "Deck"]
    curr_deck_cards <- curr_cards[cycler_id == CYCLER_ID & Zone == "Deck"]
    #count cards again
    card_count <-   nrow(curr_deck_cards)
    recycle_count <- 0
  }

  #options, in deck cards >= 4, take random 4.
  #in deck cards < 4, but deck+recycle > 4. draw first, shuffle, draw rest
  #in deck +recycle <= 4. draw all
  #in deck+recycle 0 cards, add exhaustion
  if (card_count >= how_many_cards) {
    res <- sample(curr_deck_cards[, row_id], size = how_many_cards)
    curr_cards[row_id %in% res, Zone := "Hand"]
  } else if (card_count < how_many_cards & (recycle_count + card_count > how_many_cards)) {

    #draw rest of deck
    curr_cards[cycler_id == CYCLER_ID & Zone == "Deck", Zone := "Hand"]
    #shuffle
    need_more_cards <- how_many_cards- card_count
    curr_cards[cycler_id == CYCLER_ID & Zone == "Recycle", Zone := "Deck"]
    curr_deck_cards <- curr_cards[cycler_id == CYCLER_ID &  Zone == "Deck"]
    rest_of_cards <- sample(curr_deck_cards[, row_id], size = need_more_cards)
    curr_cards[row_id %in% rest_of_cards, Zone := "Hand"]

  } else if (card_count + recycle_count == 0) {
    #out of deck, add exhaustion
    curr_cards <- add_exhaustion(cycler_id, curr_cards, "Hand")

  } else {
    #draw all
    curr_cards[cycler_id == CYCLER_ID & Zone %in% c("Deck", "Recycle"), Zone := "Hand"]
  }
  return(curr_cards)

}
