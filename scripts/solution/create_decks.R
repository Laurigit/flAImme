#create decks
#cyclers <- c(1,2,3,4,5,6)
#required_data(c("STG_DECK", "ADM_CYCLER_DECK"))
create_decks <- function(cyclers, ADM_CYCLER_DECK, extra_exhaust = NULL, breakaway_data = NULL) {

  res <- ADM_CYCLER_DECK[CYCLER_ID %in% cyclers, .(CYCLER_ID, CARD_ID, Count, Zone = "Deck", MOVEMENT)]
  if  (!is.null(extra_exhaust)) {
    if (sum(extra_exhaust) > 0) {
      cyc_exh <- data.table(CYCLER_ID = cyclers, CARD_ID = 1, Count = extra_exhaust, Zone = "Deck", MOVEMENT = 2)[Count > 0]
      res <- rbind(res, cyc_exh)

    }
  }

  if (!is.null(breakaway_data)) {
    #CYCLER_ID, MOVEMENT, bid_count
    aggr_first <- breakaway_data[, .(bid_count = .N), by = .(MOVEMENT, CYCLER_ID)]
    join_to_res <- aggr_first[res, on = .(CYCLER_ID, MOVEMENT)]
    join_to_res[!is.na(bid_count), Count := Count - bid_count]
    #add exhaust
    ba_cyclers <- aggr_first[, CYCLER_ID]
    cyc_exh <- data.table(CYCLER_ID = ba_cyclers, CARD_ID = 1, Count = extra_exhaust, Zone = "Deck", MOVEMENT = 2)[Count > 0]
    join_to_res[, bid_count := NULL]
    res_temp <- rbind(join_to_res, cyc_exh)
    #aggregate over multiple exhaust rows

    res <- res_temp[, .(Count = sum(Count)), by = .(CYCLER_ID, CARD_ID, Zone, MOVEMENT)]
  }


  spread <- res[rep(1:.N,Count)][,index:=1:.N,by=CARD_ID][, index := NULL][, Count := NULL]
  spread[, row_id := seq_len(.N)]
  return(spread)
}
