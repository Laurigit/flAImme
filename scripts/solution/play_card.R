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
play_card <- function(cycler_id, card_id, current_decks, game_id, turn_id, con = FALSE, card_row_id = NULL) {
  if (is.null(card_row_id)) {
    card_played <- current_decks[CARD_ID == card_id & CYCLER_ID == cycler_id & Zone == "Hand", max(row_id)]
  } else {
    card_player <- card_row_id
  }

  current_decks[row_id == card_played, Zone := "Removed"]
  #set rest to recycle
  current_decks[cycler_id == CYCLER_ID & Zone == "Hand", Zone := "Recycle"]
  if (con != FALSE) {
    row <- data.table(CYCLER_ID, CARD_ID, GAME_ID = game_id, TURN_ID = turn_id)
    dbWriteTable(con, "MOVE_FACT", row, append = TRUE, row.names = FALSE)
  }

return(current_decks)

}
