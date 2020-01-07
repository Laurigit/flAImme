#draw cards
#current_decks_input <- create_decks(input_STARTUP_DATA[, CYCLER_ID], ADM_CYCLER_DECK)

#test <- draw_cards(1, current_decks_input)

calculate_potential_next_draw <- function(cycler_id, current_decks_input, how_many_cards = 4) {

  #1 check if cards left in deck
  #for_debug <- current_decks_input#[cycler_id == CYCLER_ID]
  curr_cards <- current_decks_input
  curr_deck_cards <- curr_cards[cycler_id == CYCLER_ID & Zone == "Deck"]

  #check if enough cards
  card_count <-  nrow(curr_deck_cards)
  recycle_count <-  curr_cards[cycler_id == CYCLER_ID & Zone == "Recycle", .N]




  #if no cards, shuffle
  if (card_count == 0) {

    #DRAW ALL RECYCLED
    if (recycle_count <= how_many_cards) {
      calc_draw_round <- 0
    } else {
      calc_draw_round <- 1
    }
    drawn_cards <-   curr_cards[cycler_id == CYCLER_ID & Zone == "Recycle",. (CYCLER_ID, CARD_ID, MOVEMENT, draw_round = calc_draw_round)]
  }

  #options, in deck cards >= 4, take random 4.
  #in deck cards < 4, but deck+recycle > 4. draw first, shuffle, draw rest
  #in deck +recycle <= 4. draw all
  #in deck+recycle 0 cards, add exhaustion
  if (card_count >= how_many_cards) {
      #DRAW ALL CARDS IN DECK, not in reccyle
    drawn_cards <- curr_cards[cycler_id == CYCLER_ID & Zone == "Deck",. (CYCLER_ID, CARD_ID, MOVEMENT, draw_round = 1)]
  } else if (card_count < how_many_cards & (recycle_count + card_count > how_many_cards)) {

    #draw rest of deck

    drawn_cards_phase1 <- curr_cards[cycler_id == CYCLER_ID & Zone == "Deck",. (CYCLER_ID, CARD_ID, MOVEMENT, draw_round = 0)]
    #draw all recycle
    drawn_cards_phase2 <- curr_cards[cycler_id == CYCLER_ID & Zone == "Recycle",. (CYCLER_ID, CARD_ID, MOVEMENT, draw_round = 1)]
    drawn_cards <- rbind(drawn_cards_phase1, drawn_cards_phase2)


  } else if (card_count + recycle_count == 0) {
    #out of deck, add exhaustion
    drawn_cards <- data.table(CYCLER_ID = cycler_id, CARD_ID = 1, MOVEMENT = 2, draw_round = 0)

  } else {
    #draw all
    drawn_cards <- curr_cards[cycler_id == CYCLER_ID & Zone %in% c("Deck", "Recycle"), . (CYCLER_ID, CARD_ID, MOVEMENT, draw_round = 0)]
  }
  return(drawn_cards)


}
