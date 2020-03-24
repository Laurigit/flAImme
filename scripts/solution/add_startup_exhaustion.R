#add exhaustion

#curr_decks <- create_decks(input_STARTUP_DATA[, CYCLER_ID], ADM_CYCLER_DECK)
#cycler_id_vect <- c(1,2,3)
add_startup_exhaustion <- function(EXH_DATA, deck_status) {
  max_row_id <- deck_status[, max(row_id)]
  copy_exh <- copy(EXH_DATA)
  copy_exh[, help_row_id := seq_len(.N)]
  copy_exh[, ':=' (CARD_ID = 1, Zone = "Deck", row_id = help_row_id + max_row_id, MOVEMENT = 2, startup_exh = round(EXHAUST_LEFT / 2))]
  spread <- copy_exh[rep(1:.N, EXHAUST_LEFT)][,index := 1:.N,by = CYCLER_ID]
  spread[, ':=' (EXHAUST_LEFT = NULL,
                 index = NULL,
                 startup_exh = NULL)]
  appendaa <- rbind(deck_status, spread)
  return(appendaa)


}
