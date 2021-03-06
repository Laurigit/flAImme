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
play_card <- function(cycler_id,
                      card_id,
                      current_decks_inpu,
                      game_id,
                      turn_id,
                      con = NULL,
                      card_row_id = NULL,
                      MOVEMENT_PLAYED = NULL,
                      force = FALSE,
                      copy = TRUE) {
#force means play it anywhere even if it is not in hand or deck

  if (copy == TRUE) {
    current_decks <- copy(current_decks_inpu)
  } else {
    current_decks <- current_decks_inpu
  }

  #find the card
  if (!is.null(MOVEMENT_PLAYED)) {
    card_id_played <- current_decks[MOVEMENT == MOVEMENT_PLAYED & CYCLER_ID == cycler_id & Zone == "Hand", min(CARD_ID)]
    #then choose one row id
    card_played <- current_decks[CARD_ID == card_id_played & CYCLER_ID == cycler_id & Zone == "Hand", max(row_id)]
  } else if (is.null(card_row_id)) {
      #in this case, we have input card_id
    card_played <- current_decks[CARD_ID == card_id & CYCLER_ID == cycler_id & Zone == "Hand", max(row_id)]

    } else {
    card_played <- current_decks[row_id == card_row_id & Zone == "Hand", row_id]
  }

  #heres the phase where we take the card from anywhere
  #if we did not find the card from hand and force is true, play it from deck

  if (is.infinite(card_played)) {
      if (force == TRUE) {
        if (!is.null(MOVEMENT_PLAYED)) {
          card_id_played <- current_decks[MOVEMENT == MOVEMENT_PLAYED & CYCLER_ID == cycler_id & Zone != "Removed", min(CARD_ID)]
          #then choose one row id
          card_played <- current_decks[CARD_ID == card_id_played & CYCLER_ID == cycler_id & Zone != "Removed", max(row_id)]
        } else if (is.null(card_row_id)) {
          #in this case, we have input card_id
          card_played <- current_decks[CARD_ID == card_id & CYCLER_ID == cycler_id & Zone != "Removed", max(row_id)]

        } else {
          card_played <- current_decks[row_id == card_row_id & Zone != "Removed", row_id]
        }
    } else {
      stop()
      #tried to play removed card
    }
  }


  current_decks[row_id == card_played, Zone := "Removed"]
  #set rest to recycle
  current_decks[cycler_id == CYCLER_ID & Zone == "Hand", Zone := "Recycle"]



  if (!is.null(con)) {
    #card_id to db
    played_id <- current_decks[row_id == card_played, CARD_ID]
      row <- data.table(CYCLER_ID = cycler_id, CARD_ID = played_id, GAME_ID = game_id, TURN_ID = turn_id)

    dbWriteTable(con, "MOVE_FACT", row, append = TRUE, row.names = FALSE)
  }

return(current_decks)

}
