blocking_odds_all_phase2 <- function(game_status, range_data, STG_CYCLER, phase_two_cyclers) {

  #range_data <- calc_move_range(take_copy_of_game, deck_status)
  cyclers <- phase_two_cyclers
  tot_data <- NULL
  for (cyc_loop in cyclers) {
    res <- odds_of_blocked_slots_phase2(game_status, range_data, STG_CYCLER, cyc_loop, phase_two_cyclers)
    tot_data <- rbind(tot_data, res)
  }


  return(tot_data[order(blocked_odds)])

}
