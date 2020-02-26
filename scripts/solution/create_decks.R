#create decks
#cyclers <- c(1,2,3,4,5,6)
#required_data(c("STG_DECK", "ADM_CYCLER_DECK"))
create_decks <- function(cyclers, ADM_CYCLER_DECK) {
  res <- ADM_CYCLER_DECK[CYCLER_ID %in% cyclers, .(CYCLER_ID, CARD_ID, Count, Zone = "Deck", MOVEMENT)]
  spread <- res[rep(1:.N,Count)][,index:=1:.N,by=CARD_ID][, index := NULL][, Count := NULL]
  spread[, row_id := seq_len(.N)]
  return(spread)
}
