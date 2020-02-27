simulate_and_scores_phase_2 <- function(simul_res, STG_CYCLER, team_id, cycler_id) {

    res <-  simul_res$scores
    #join_team_to_res
    res_team <- STG_CYCLER[res, on = "CYCLER_ID"]

    action_data <- res_team[CYCLER_ID == cycler_id, .N, by = .(TEAM_ID, CYCLER_ID, MOVEMENT, simul_round)]


    all_scores <- res_team[CYCLER_ID == cycler_id,.(Score = sum(Result)), by = .(TEAM_ID, simul_round)][order(Score)]

    #join action and score
    act_score <- action_data[all_scores, on = .(TEAM_ID, simul_round)]


    #total_scores[, .(mean_score = mean(Score, na.rm = TRUE), .N), by = .(CYCLER_ID,TEAM_ID,MOVEMENT)][order(-mean_score)]

  score_data <- act_score[, .(mean_score = mean(Score, na.rm = TRUE), .N), by = .(CYCLER_ID, TEAM_ID,MOVEMENT)][order(-mean_score)]
  print(score_data)
  my_best_score <- score_data[TEAM_ID == team_id, max(mean_score)]
  my_decision <- score_data[mean_score == my_best_score][1,. (CYCLER_ID, MOVEMENT)]
  return(my_decision)
}
