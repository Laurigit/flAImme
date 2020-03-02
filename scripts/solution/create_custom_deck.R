create_custom_deck <- function(cycler_id, id_vec, zone_vec, ADM_CYCLER_DECK) {

  ZONE_MAP <- data.table(Zone = c("Deck", "Hand", "Recycled", "Removed"), Zone_id = c(1, 2, 3, 4))
    dt_zone <- data.table(Zone_id = zone_vec)
    id_dt <-data.table(CARD_ID =  id_vec)
  join_zone_vec <- ZONE_MAP[dt_zone, on = "Zone_id"]
  res <- STG_CARD[id_dt, on = "CARD_ID"][, .(CYCLER_ID = cycler_id, CARD_ID, Count = 1, Zone = join_zone_vec[, Zone], MOVEMENT)]


  spread <- res[rep(1:.N,Count)][,index:=1:.N,by=CARD_ID][, index := NULL][, Count := NULL]
  spread[, row_id := seq_len(.N)]
  return(spread)
}
