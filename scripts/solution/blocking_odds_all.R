blocking_odds_all <- function(game_status, range_data, STG_CYCLER) {

#range_data <- calc_move_range(take_copy_of_game, deck_status)
cyclers <- game_status[CYCLER_ID > 0, CYCLER_ID]
tot_data <- NULL
for (cyc_loop in cyclers) {
  res <- odds_of_blocked_slots(game_status, range_data , STG_CYCLER, cyc_loop)
  tot_data <- rbind(tot_data, res)
}


return(tot_data[order(blocked_odds)])

}
