ttf_bot <- function(team_combinations_data_with_other_player_probs, deck_status,
                           bot_config, bot_team_id, pre_aggr_game_status_input) {

  #team_combinations_data_with_other_player_probs <- hidden_information_output

  join_my_ttf <- team_combinations_data_with_other_player_probs
  min_ttf <- join_my_ttf[, min(TURNS_TO_FINISH)]

  #calc my ev given opponent strategies
  finish_slot <- pre_aggr_game_status_input[FINISH == 1, GAME_SLOT_ID]

  #setkeyv sorts cyclers by case_id and new_square

  # join_my_ttf[, ':=' (MY_TEAM_ROW = ifelse(CYCLER_ID %in% smart_cycler_ids, 1,NA))]
  # join_my_ttf[, ':=' (MY_TEAM_MIN_TTF = min(TURNS_TO_FINISH * MY_TEAM_ROW, na.rm = TRUE)), by = case_id]
  #-1 so that we include cases where we push opponent back
  # join_my_ttf[, ':=' (RELEVANT_OPPONENT = MY_TEAM_MIN_TTF >= (TURNS_TO_FINISH - 1)), by = case_id]
  #join_my_ttf[, ':=' (CYCLER_MEAN_TTF = mean(TURNS_TO_FINISH)), by = .(CYCLER_ID)]
  #  join_my_ttf[, ':=' (TTF_DIFF_OF_MEAN = CYCLER_MEAN_TTF - TURNS_TO_FINISH)]
  #  join_track_left[, ':=' (TEAM_TTF_MEAN = mean(TURNS_TO_FINISH), cycs = .N), by = .(case_id, TEAM_ID)]
  #   join_my_ttf[, ':=' (CASE_DIFF = sum(TTF_DIFF_OF_MEAN * RELEVANT_OPPONENT), tot_cycs = sum(RELEVANT_OPPONENT)), by = .(case_id)]
  #  join_my_ttf[, ':=' (COMPETITOR_AVG_TTF = (CASE_DIFF - TTF_DIFF_OF_MEAN) / (tot_cycs - 1))]
  join_my_ttf[, ':='(RELATIVE_TTF = (CYCLER_MEAN_TTF - TURNS_TO_FINISH))]#positive is good
  join_my_ttf[, ':='(RELATIVE_SOF = (SLOTS_OVER_FINISH  - CYCLER_MEAN_SOF))]
  join_my_ttf[, ':=' (SLOTS_PROGRESSED = NEW_GAME_SLOT_ID - curr_pos)]
  join_my_ttf[, ':=' (MOVE_ORDER = seq_len(.N),
                      MOVE_DIFF_RELATIVE = MOVE_DIFF - (sum(MOVE_DIFF) - MOVE_DIFF) / (.N - 1)), by = case_id]



  # join_track_left[, MOVE_ORDER := seq_len(.N), by = case_id]
  setkeyv(join_my_ttf, cols = c("case_id", "TEAM_ID", "CYCLER_ID"))

  # join_track_left[, MOVE_DIFF_RELATIVE := MOVE_DIFF - (sum(MOVE_DIFF) - MOVE_DIFF) / (.N - 1), by = case_id]

  #filter_only_my_team_for_scoring
  scoring_data <- join_my_ttf[TEAM_ID == bot_team_id]
  scoring_data[, ':=' (MOVE_DIFF_SCORE = MOVE_DIFF_RELATIVE * 0.25 * pmax(min_ttf - 4, 0.1),
                       EXHAUST_SCORE = ((1 + MOVE_ORDER) / total_cyclers) * EXHAUST * pmax(min_ttf - 3, 0) ^ 1.2 * -0.2,
                       #   TTF_SCORE = RELATIVE_TTF * 20 * ((total_cyclers - max(MOVE_ORDER, 4)) / total_cyclers),
                       TTF_SCORE = RELATIVE_TTF * 5,
                       SOF_SCORE = RELATIVE_SOF,
                       # CYC_DIST_SCORE = DIST_TO_TEAM * - 0.03 * pmax(TURNS_TO_FINISH - 3, 0),
                       MOVE_ORDER_SCORE = - MOVE_ORDER * 0.02 * (22 - min_ttf),
                       OVER_FINISH_SCORE = OVER_FINISH * 100,
                       SLOTS_PROGRESSED_SCORE = SLOTS_PROGRESSED * 0.001 * (16 - min_ttf))]

  scoring_data[, TOT_SCORE := (MOVE_DIFF_SCORE +
                                 EXHAUST_SCORE +
                               #  SOF_SCORE +
                                 TTF_SCORE +
                                 #   CYC_DIST_SCORE +
                                 SLOTS_PROGRESSED_SCORE +
                                 MOVE_ORDER_SCORE +
                                 OVER_FINISH_SCORE)]

  #join_opponent strat
  # ss_strat <- MIXED_STRATEGY[, .(MOVES, TEAM_ID, PROB_PRODUCT)]
  # join_strat <- ss_strat[join_my_ttf, on = .(MOVES, TEAM_ID)]
  #
  # ss_for_picture <- join_strat[, .(TURNS_TO_FINISH, CYCLER_ID, PROB_PRODUCT, case_id, NEW_GAME_SLOT_ID)]
  # ss_for_picture[, CASE_PROB := prod(PROB_PRODUCT^(1/2)), by = case_id]
  #
  # aggr_pic <- ss_for_picture[, .(TURNS_TO_FINISH = sum(TURNS_TO_FINISH * CASE_PROB) / sum(CASE_PROB),
  #                                NEW_GAME_SLOT_ID = sum(NEW_GAME_SLOT_ID  * CASE_PROB) / sum(CASE_PROB)), by = CYCLER_ID]
  #
  # aggr_pic[, TURN_ID := turn_id]
  # if (turn_id == 1) {
  #   acnhor_turn <- aggr_pic[, min(TURNS_TO_FINISH)]
  # }
  # aggr_pic[, TTF_SCALED := TURNS_TO_FINISH - acnhor_turn + TURN_ID - 1, by = .(TURN_ID)]
  # TTF_stats <- rbind(TTF_stats, aggr_pic)
  # finishssi <- game_status[FINISH == 1, min(GAME_SLOT_ID)]
  # startti <-game_status[START == 1, min(GAME_SLOT_ID)] + 2
  # print(ggplot(data=TTF_stats, aes(x=NEW_GAME_SLOT_ID, y=TTF_SCALED, group=CYCLER_ID)) +
  #         #geom_line(linetype="dashed", color="blue", size=1.2)+
  #         geom_line(size=2.5, aes(linetype = "solid", color=as.factor(CYCLER_ID)))+
  #         geom_point(size = 3, aes(color=as.factor(CYCLER_ID))) +
  #         xlim(6, finishssi) + ylim(-1.5, 2.2)) + scale_x_continuous(limits = c(finishssi-70, finishssi), breaks = seq(finishssi-70, finishssi, by = 10))
  #
  #
  #
  #weight my cyclers based on ttf
  scoring_data[, CYCLER_WEIGHT := (1 - CYCLER_MEAN_TTF / (sum(CYCLER_MEAN_TTF) + 0.1)), by = .(TEAM_ID, case_id)]
  scoring_data[, CYCLER_WEIGHT := ifelse(CYCLER_WEIGHT == 0, 1, CYCLER_WEIGHT)]

  check_res <- scoring_data[, .(TEAM_SCORE = sum(TOT_SCORE * CYCLER_WEIGHT),
                                MOVE_DIFF_SCORE  = sum(MOVE_DIFF_SCORE * CYCLER_WEIGHT),
                                EXHAUST_SCORE = sum(EXHAUST_SCORE * CYCLER_WEIGHT),
                                SLOTS_PROGRESSED_SCORE = sum(SLOTS_PROGRESSED_SCORE * CYCLER_WEIGHT),
                                TTF_SCORE = sum(TTF_SCORE * CYCLER_WEIGHT),
                                TURNS_TO_FINISH = mean(TURNS_TO_FINISH * CYCLER_WEIGHT),
                                SOF_SCORE = mean(SOF_SCORE * CYCLER_WEIGHT),
                                #     CYC_DIST_SCORE = sum(CYC_DIST_SCORE),
                                MOVE_ORDER_SCORE = sum(MOVE_ORDER_SCORE * CYCLER_WEIGHT),
                                OVER_FINISH_SCORE = sum(OVER_FINISH_SCORE * CYCLER_WEIGHT)), by = .(MOVES, TEAM_ID, case_id, PROB_PRODUCT, CYCLERS, OPPONENT_PROB)]

  # check_res[, OPPONENT_CASE_PROB := prod(PROB_PRODUCT) / PROB_PRODUCT, by = case_id]
  # check_res[, OPPONENT_CASE_PROB := ifelse(is.na(OPPONENT_CASE_PROB), 0.000001, OPPONENT_CASE_PROB)]
  #

  check_res2 <- check_res[, .(EV = sum(TEAM_SCORE * OPPONENT_PROB) / sum(OPPONENT_PROB),
                              MOVE_DIFF_SCORE  = sum(MOVE_DIFF_SCORE * OPPONENT_PROB) / sum(OPPONENT_PROB),
                              EXHAUST_SCORE = sum(EXHAUST_SCORE * OPPONENT_PROB) / sum(OPPONENT_PROB),
                              SLOTS_PROGRESSED_SCORE = sum(SLOTS_PROGRESSED_SCORE * OPPONENT_PROB) / sum(OPPONENT_PROB),
                              TTF_SCORE = sum(TTF_SCORE * OPPONENT_PROB) / sum(OPPONENT_PROB),
                              TURNS_TO_FINISH = sum(TURNS_TO_FINISH * OPPONENT_PROB) / sum(OPPONENT_PROB),
                              SOF_SCORE = sum(SOF_SCORE * OPPONENT_PROB) / sum(OPPONENT_PROB),
                              #   CYC_DIST_SCORE = mean(CYC_DIST_SCORE),
                              MOVE_ORDER_SCORE = sum(MOVE_ORDER_SCORE * OPPONENT_PROB) / sum(OPPONENT_PROB),
                              OVER_FINISH_SCORE = sum(OVER_FINISH_SCORE * OPPONENT_PROB) / sum(OPPONENT_PROB)
  ), by = .(CYCLERS, MOVES, TEAM_ID)][order(-EV)]

  #   best_move_diff <- check_res2[which.max(MOVE_ORDER_SCORE), MOVES]
  #   best_sof_diff <- check_res2[which.max(SOF_SCORE), MOVES]
  # #  print(check_res2)
  #   if (best_move_diff != best_sof_diff) {
  #     browser()
  #   }
  #   #  scalued <- check_res2[ ,.(MOVES, EV = round(EV - max(EV), 2),
  # MOVE_DIFF_SCORE = round(MOVE_DIFF_SCORE, 2),
  # EXHAUST_SCORE = round(EXHAUST_SCORE - max(EXHAUST_SCORE), 2),
  # SLOTS_PROGRESSED_SCORE = round(SLOTS_PROGRESSED_SCORE - max(SLOTS_PROGRESSED_SCORE), 2),
  # TTF_SCORE = round(TTF_SCORE, 2),
  # TURNS_TO_FINISH = round(TURNS_TO_FINISH, 2),
  # MOVE_ORDER_SCORE = round(MOVE_ORDER_SCORE - mean(MOVE_ORDER_SCORE), 2),
  # OVER_FINISH_SCORE = round(OVER_FINISH_SCORE, 2))]
  # print(scalued[1:10][order(-EV)])
  return(check_res2)
}

