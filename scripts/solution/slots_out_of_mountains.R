slots_out_of_mountains <- function(game_state) {
  # game_state[, rleidi := rleid(CYCLER_ID)]

  sscols <- game_state[, .(mountain_row = ifelse(PIECE_ATTRIBUTE == "M", 1, 0), GAME_SLOT_ID, CYCLER_ID)]
  sscols_aggr <- sscols[, .N, by = .(mountain_row, GAME_SLOT_ID)][order(-GAME_SLOT_ID)]
  sscols_aggr[, start_of_restricted_movement := ifelse(shift(mountain_row == 1, n = 6) | mountain_row == 1, 1, 0)]
  sscols_aggr[, start_of_restricted_movement := ifelse(is.na(start_of_restricted_movement), 0, start_of_restricted_movement)]
  sscols_aggr[, counter_cons_piece := rowid(rleid(start_of_restricted_movement))]

  sscols_aggr[, turns_ouf_of_mountain_temp := ifelse(start_of_restricted_movement == 1, ceiling(counter_cons_piece / 5), NA)]
  max_out_mountain <- 0
  prev_arvo <- 0
  res_vector <- 1:nrow(sscols_aggr)
  for (slotit in 1:nrow(sscols_aggr)) {
    rivitieto <- sscols_aggr[slotit, turns_ouf_of_mountain_temp]
    if (is.na(rivitieto)) {
      res_vector[slotit] <- prev_arvo
      max_out_mountain <- prev_arvo
    } else {
      if (prev_arvo < rivitieto) {
        res_vector[slotit] <- prev_arvo + 1 +max_out_mountain
      } else {
        res_vector[slotit] <- rivitieto + max_out_mountain
       }

    }
    prev_arvo <- res_vector[slotit]
  }

  sscols_aggr[, turns_out_of_mountain := res_vector]
  sscols_res <- sscols_aggr[, .(GAME_SLOT_ID, turns_out_of_mountain)]
  joinaa <- sscols_res[game_state, on = "GAME_SLOT_ID"]

  return(joinaa)
}
