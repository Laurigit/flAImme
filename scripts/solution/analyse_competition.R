analyse_competition <- function(game_status, deck_status, strategy_f_input, aim_dh_input, loop_count = 10, full_strategy_table = NULL) {
  #strategy_f_input <- c("MAX", "SMART_MAX")
  #aim_dh_input <- c(0 ,1)
  #diasable blockign


  if (is.null(full_strategy_table)) {
    #wide_game <- add_lanes_to_track(game_status)
    cyclers <- game_status[CYCLER_ID > 0, .(CYCLER_ID, KEY = "1")]
    strats <- data.table(strategy = strategy_f_input , aim_dh = aim_dh_input, KEY = "1")
    joinaa <- merge(x= strats, y = cyclers, by = "KEY", all = TRUE, allow.cartesian = TRUE)
  } else {
    joinaa <- full_strategy_table
    joinaa[, KEY := "1"]
  }

  simulations <- data.table(simul = 1:loop_count, KEY = "1")
  joinsim <- joinaa[simulations, on ="KEY", allow.cartesian = TRUE]
  joinsimcols <- joinsim[,. (strategy, aim_dh, CYCLER_ID, simul = seq_len(.N), TURN_ID = 0, POSITION = 0, GAME_ID = 0, row_over_finish = 0, finish_square = 0 )]
  total_res <- NULL
  for (simloop in 1:nrow(joinsimcols)) {

    row_input <- joinsimcols[simloop]
    strat_input <- row_input[, strategy]
    dh_input <- row_input[, aim_dh]
    loop_cycler <- row_input[, CYCLER_ID]
    res <- cbind(strategy = strat_input, aim_dh = dh_input,simulate_strategy_with_random_cards(game_status, deck_status, loop_cycler, strategy = strat_input, aim_downhill_count = dh_input))
    joinsimcols[simul== simloop, colnames(res) := res]
    print(joinsimcols[TURN_ID > 0])
  }



  joinsimcols[, KPI := TURN_ID*100 - row_over_finish]
  #joinsimcols[, mean(KPI), by = strategy]
  #joinsimcols[, mean(KPI), by = aim_dh]
  #joinsimcols[, mean(KPI), by = CYCLER_ID]
  #joinsimcols[order(-KPI)]
  return(joinsimcols)
}
