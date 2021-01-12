#calc new mixed strategy
calculated_mix_strategy_inner <- function(input_current_ev_data, gamma = 0.9) {
 current_ev_data <- copy(input_current_ev_data)
  current_ev_data[, top := exp(1/gamma*EV) ]
  current_ev_data[, bottom := sum(top), by = TEAM_ID]
  current_ev_data[, new_prob_uncapped := top / bottom]
  current_ev_data[, new_prob_provisional := top / bottom]
  #check if results are acceptable
  acceptable <- current_ev_data[, min(new_prob_provisional <= MIX_STRATEGY_CAP)]
  safety_counter <- 0
  while ( acceptable == 0) {
    safety_counter <- safety_counter +1
    current_ev_data[, new_prob_capped := pmin(new_prob_provisional, MIX_STRATEGY_CAP)]

    #now need to scale back to 1
    current_ev_data[, missing_prob :=  1 - sum(new_prob_capped), by = TEAM_ID]
    current_ev_data[, non_missing_prob_mass :=  sum(ifelse(new_prob_capped == MIX_STRATEGY_CAP, 0, new_prob_capped)), by = TEAM_ID]
    #scaling the uncapped
    current_ev_data[, new_prob_provisional := ifelse(new_prob_capped == MIX_STRATEGY_CAP, new_prob_capped, new_prob_provisional  + new_prob_provisional /  non_missing_prob_mass * missing_prob)]

    acceptable <- current_ev_data[, min(new_prob_provisional <= MIX_STRATEGY_CAP)]
    if (safety_counter > 10) {acceptable <- 1}
  }
  setnames(current_ev_data, "new_prob_provisional", "new_prob")

  mixed_strategy <- current_ev_data[, .(CYCLERS, MOVES, TEAM_ID, new_prob)]


  join_new_strat <- mixed_strategy[res, on = .(CYCLERS, MOVES, TEAM_ID)]
  join_new_strat[, NEW_CASE_PROB := prod(new_prob)]
  current_ev_data <- join_new_strat[, .(TEAM_SCORE = sum(TOT_SCORE)), by = .(MOVES, CYCLERS, TEAM_ID, case_id, opponent_moves, new_prob, MIX_STRATEGY_CAP)]

  current_ev_data[, OPPONENT_CASE_PROB := prod(new_prob) / new_prob, by = case_id]

  current_ev_data[, WEIGHTED_SCORE := OPPONENT_CASE_PROB * TEAM_SCORE]

  result <- current_ev_data[, .(EV = sum(WEIGHTED_SCORE)), by = .(CYCLERS, MOVES, TEAM_ID, MIX_STRATEGY_CAP, PROB_PRODUCT = new_prob)]
  return(result)
}
