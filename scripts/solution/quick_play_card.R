#play card

# current_decks  <- create_decks(input_STARTUP_DATA[, CYCLER_ID], ADM_CYCLER_DECK)
# current_decks <-  draw_cards(2, current_decks)
# kasi <- current_decks[CYCLER_ID == 2 & Zone == "Hand", CARD_ID]
# current_decks <- play_card(2, max(kasi), current_decks, 1, 1, FALSE)
# current_decks[CYCLER_ID == 2]
#
#
#
# cycler_id <- 2
# card_id <- 9
quick_play_card <- function(cycler_id, card_id, current_decks_inpu) {
  #force means play it anywhere even if it is not in hand or deck



    card_played <- current_decks_inpu[CARD_ID == card_id & CYCLER_ID == cycler_id & Zone == "Hand", max(row_id)]



    current_decks_inpu[row_id == card_played, Zone := "Removed"]
  #set rest to recycle
    current_decks_inpu[cycler_id == CYCLER_ID & Zone == "Hand", Zone := "Recycle"]


  return(current_decks_inpu)

}
