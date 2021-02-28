#add exhaustion

#curr_decks <- create_decks(input_STARTUP_DATA[, CYCLER_ID], ADM_CYCLER_DECK)
#cycler_id_vect <- c(1,2,3)
add_startup_exhaustion <- function(EXH_DATA, deck_status) {
browser()
    max_row_id <- deck_status[, max(row_id)]
  copy_exh[,  max_row := max(row_id), by = CYCLER_ID]
  #copy_exh[, help_row_id := seq_len(.N)]
  copy_exh[, ':=' (CARD_ID = 1, Zone = "Deck", MOVEMENT = 2, startup_exh = round(EXHAUST_LEFT / 2))]
  spread <- copy_exh[rep(1:.N, startup_exh)][,index := 1:.N,by = CYCLER_ID]
  spread[, row_id := max_row_id + seq_len(.N)]
  #JOIN_MAX <- max_row_id[spread, on = "CYCLER_ID"]
  spread[, ':=' (EXHAUST_LEFT = NULL,
                 index = NULL,
                 startup_exh = NULL,
                 help_row_id = NULL)]

  appendaa <- rbind(deck_status, spread)
  return(appendaa)


}
