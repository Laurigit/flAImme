which_cycler_to_move_first <- function(simul_scores, STG_CYCLER) {




  action_data <- simul_scores[, .N, by = .(MOVEMENT, CYCLER_ID, simul_round, phase)]
    team_decision <- action_data[phase == 1, .(CYCLER_ID, MOVEMENT, simul_round)]
    join_team_to_dec <- STG_CYCLER[team_decision, on = "CYCLER_ID"]
    #join_team
    join_team <- STG_CYCLER[simul_scores, on = "CYCLER_ID"]

    all_scores <- join_team[,.(Score = sum(Result)), by = .(TEAM_ID, simul_round)][order(Score)]
    join_dec <- join_team_to_dec[all_scores, on = .(TEAM_ID, simul_round)]

    #total_scores[, .(mean_score = mean(Score, na.rm = TRUE), .N), by = .(CYCLER_ID,TEAM_ID,MOVEMENT)][order(-mean_score)]

  score_data <- join_dec[, .(mean_score = mean(Score, na.rm = TRUE), .N), by = .(CYCLER_ID,TEAM_ID,MOVEMENT)][order(-mean_score)]
  #aggregate over moves
  score_data_over_moves <- score_data[is.finite(mean_score), .(weighted_score = sum(mean_score * N) / sum(N)), by = .(CYCLER_ID, TEAM_ID)]

  my_best_score <- score_data_over_moves[TEAM_ID == team_id, max(weighted_score)]
  my_decision <- score_data_over_moves[weighted_score == my_best_score][1, CYCLER_ID]


  return(my_decision)
}
