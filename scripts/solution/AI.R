
#game_state <- game_status
AI <- function(cycler_id, game_state, deck_status) {

  #estimate best strategy for all
  cyclers <- game_state[CYCLER_ID > 0, CYCLER_ID]
  best_strategy_per_cycler <-  data.table(rbindlist(lapply(cyclers, function(x) {estimate_best_strategy(game_state, deck_status, cycler_id = x)})))

  input_strat_table <- best_strategy_per_cycler[, .(CYCLER_ID, aim_dh = dh_count, strategy)]
  res <- analyse_competition(game_status, deck_status, loop_count =  3, full_strategy_table = input_strat_table)
  res[, KPI := TURN_ID*10000 + (10 - row_over_finish)]
  res_kpi <- res[, .(Mean_turn = mean(TURN_ID), mean_row = mean(row_over_finish), KPI = mean(TURN_ID)*10 - mean(row_over_finish)), by = CYCLER_ID]
  best_comps <- res_kpi[which.min(KPI)]
  return(best_comps)


  #choose strategy
  # sprint
  # 2nd
  # slip
  # survive / catch peloton
  # support
  # save teammate
  #

  #sprint if missing moves is less than X


}
