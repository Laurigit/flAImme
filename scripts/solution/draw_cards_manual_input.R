#draw cards
#current_decks_input <- create_decks(input_STARTUP_DATA[, CYCLER_ID], ADM_CYCLER_DECK)

#test <- draw_cards(1, current_decks_input)

draw_cards_manual_input <- function(cycler_id, current_decks_input, drawn_cards_vec, how_many_cards) {

  #1 check if cards left in deck
  #for_debug <- current_decks_input#[cycler_id == CYCLER_ID]
  curr_cards <- current_decks_input
  curr_deck_cards <- curr_cards[cycler_id == CYCLER_ID & Zone == "Deck"]

  #check if enough cards
  card_count <-  nrow(curr_deck_cards)
  recycle_count <-  curr_cards[cycler_id == CYCLER_ID & Zone == "Recycle", .N]




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
    all_row_id <- NULL
    for (card_loop in drawn_cards_vec) {
      card_row_id <- curr_deck_cards[CARD_ID == card_loop & !row_id %in% all_row_id , min(row_id)]
      all_row_id <- c(card_row_id, all_row_id)
    }
    curr_cards[row_id %in% all_row_id, Zone := "Hand"]
  } else if (card_count < how_many_cards & (recycle_count + card_count > how_many_cards)) {


    #shuffle
    curr_cards[cycler_id == CYCLER_ID & Zone == "Recycle", Zone := "Deck"]
    curr_deck_cards <- curr_cards[cycler_id == CYCLER_ID &  Zone == "Deck"]
    all_row_id <- NULL
    for (card_loop in drawn_cards_vec) {
      card_row_id <- curr_deck_cards[CARD_ID == card_loop & !row_id %in% all_row_id, min(row_id)]
      all_row_id <- c(card_row_id, all_row_id)
    }
    curr_cards[row_id %in% all_row_id, Zone := "Hand"]

  } else if (card_count + recycle_count == 0) {
    #out of deck, add exhaustion
    curr_cards <- add_exhaustion(cycler_id, curr_cards, "Hand")

  } else {
    #draw all
    all_row_id <- NULL
    for (card_loop in drawn_cards_vec) {
      card_row_id <- curr_deck_cards[CARD_ID == card_loop, min(row_id)]
      all_row_id <- c(card_row_id, all_row_id)
    }
    curr_cards[row_id %in% all_row_id, Zone := "Hand"]
  }
  return(curr_cards)

}
