simulate_and_scores_phase_1 <- function(simul_score, STG_CYCLER, cycler_id) {

  #card_options_in_hand <- c(4,5)


#join_team
 join_team <- STG_CYCLER[simul_score, on = "CYCLER_ID"]

 total_scores <- join_team[,.(Score = sum(Result)), by = .(CYCLER_ID, MOVEMENT, simul_round)][order(Score)]
#print(join_team[CYCLER_ID == 3])

#total_scores[, .(mean_score = mean(Score, na.rm = TRUE), .N), by = .(CYCLER_ID,TEAM_ID,MOVEMENT)][order(-mean_score)]


  ssCols_old <- total_scores[CYCLER_ID == cycler_id & MOVEMENT, .(CYCLER_ID, MOVEMENT, Score)]


 score_data <- ssCols_old[CYCLER_ID == cycler_id, .(mean_score = mean(Score, na.rm = TRUE), .N), by = .(MOVEMENT)][order(-mean_score)]
#print(score_data)
   my_best_score <- score_data[, max(mean_score)]
my_decision <- score_data[mean_score == my_best_score][1,  MOVEMENT]
  return(my_decision)
}
