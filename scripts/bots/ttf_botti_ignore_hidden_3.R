ttf_botti_ignore_hidden_3 <- function(team_combinations_data_with_other_player_probs, deck_status,
                                    bot_config, bot_team_id, pre_aggr_game_status_input = NULL , input_turn_id = NULL) {

  required_data("ADM_CYCLER_INFO")
  ss_info <- ADM_CYCLER_INFO[, .(CYCLER_ID, IS_ROULER = ifelse(CYCLER_TYPE_ID == 1, 1, 0))]

  exh_count <- deck_status[Zone != "Removed", .(card_count = .N, exh_count = sum(CARD_ID == 1)), by = CYCLER_ID]
  exh_count[, norm_card_share := 1 - exh_count / (card_count + 4)]
  ss_exh <- exh_count[, .(norm_card_share, CYCLER_ID)]
  #team_combinations_data_with_other_player_probs <- hidden_information_output

  join_my_ttf <- copy(team_combinations_data_with_other_player_probs)
  # join_my_ttf[, ':=' (TURNS_TO_FINISH = ifelse(bot_team_id == TEAM_ID & !is.na(TURNS_TO_FINISH_DOD), TURNS_TO_FINISH_DOD, TURNS_TO_FINISH),
  #                     SLOTS_OVER_FINISH = ifelse(bot_team_id == TEAM_ID & !is.na(SLOTS_OVER_FINISH_DOD), SLOTS_OVER_FINISH_DOD, SLOTS_OVER_FINISH))]

  min_ttf <- max(15 - (input_turn_id + 1), 1)
  #min_ttf_new <- join_my_ttf[, ceiling(mean(TURNS_TO_FINISH_NEW))]
  #calc my ev given opponent strategies

  finish_slot <- pre_aggr_game_status_input[FINISH == 1, GAME_SLOT_ID]

  #setkeyv sorts cyclers by case_id and new_square

  # join_my_ttf[, ':=' (MY_TEAM_ROW = ifelse(CYCLER_ID %in% smart_cycler_ids, 1,NA))]
  # join_my_ttf[, ':=' (MY_TEAM_MIN_TTF = min(TURNS_TO_FINISH * MY_TEAM_ROW, na.rm = TRUE)), by = case_id]
  #-1 so that we include cases where we push opponent back
  # join_my_ttf[, ':=' (RELEVANT_OPPONENT = MY_TEAM_MIN_TTF >= (TURNS_TO_FINISH - 1)), by = case_id]

  #  join_my_ttf[, ':=' (TTF_DIFF_OF_MEAN = CYCLER_MEAN_TTF - TURNS_TO_FINISH)]
  #
  #join_my_ttf[, ':=' (CASE_DIFF = sum(TTF_DIFF_OF_MEAN * RELEVANT_OPPONENT), tot_cycs = sum(RELEVANT_OPPONENT)), by = .(case_id)]
  #  join_my_ttf[, ':=' (COMPETITOR_AVG_TTF = (CASE_DIFF - TTF_DIFF_OF_MEAN) / (tot_cycs - 1))]


  join_my_ttf[, ':='(RELATIVE_TTF = (CYCLER_MEAN_TTF - TURNS_TO_FINISH))]#positive is good
  join_my_ttf[, ':='(RELATIVE_SOF = (SLOTS_OVER_FINISH  - CYCLER_MEAN_SOF))]





  # join_track_left[, MOVE_ORDER := seq_len(.N), by = case_id]
  setkeyv(join_my_ttf, cols = c("case_id", "TEAM_ID", "CYCLER_ID"))

  # join_track_left[, MOVE_DIFF_RELATIVE := MOVE_DIFF - (sum(MOVE_DIFF) - MOVE_DIFF) / (.N - 1), by = case_id]

  #filter_only_my_team_for_scoring

  mincase <- join_my_ttf[, min(case_id)]
  # print(join_my_ttf[mincase == case_id])
  total_cyclers <- nrow(join_my_ttf[, .N, by = CYCLER_ID])

  cyc_info_to_scoring <- ss_info[join_my_ttf, on = "CYCLER_ID"]

  scoring_data <- ss_exh[cyc_info_to_scoring, on = "CYCLER_ID"]#[TEAM_ID == bot_team_id]
  scoring_data[, MY_TTF := mean(TURNS_TO_FINISH - SLOTS_OVER_FINISH / 10), by = CYCLER_ID]
  mean_ttf <- scoring_data[, mean(TURNS_TO_FINISH)] + 0.01
  scoring_data[, ':=' (MOVE_DIFF_SCORE = (MOVE_DIFF - IS_ROULER * MOVE_DIFF * 0.2) * 0.2,
                       EXHAUST_SCORE =  (1 - EXHAUST) * max((1 - input_turn_id / 15), 0.01) * (3 - FINISH_RANK + 0.1) / 3,
                       #   TTF_SCORE = RELATIVE_TTF * 20 * ((total_cyclers - max(MOVE_ORDER, 4)) / total_cyclers),
                       TTF_SCORE = (mean_ttf - (TURNS_TO_FINISH - SLOTS_OVER_FINISH / 6)) * 3,
                       SOF_SCORE = 0,
                       FINISH_RANK_SCORE = FINISH_RANK,
                       # CYC_DIST_SCORE = DIST_TO_TEAM * - 0.03 * pmax(TURNS_TO_FINISH - 3, 0),
                       MOVE_ORDER_SCORE = -0.3 * MOVE_ORDER, # 1 - (MOVE_ORDER / total_cyclers) , #- MOVE_ORDER * 0.015 * (17 - min_ttf) * IS_ROULER * 0.5,
                       OVER_FINISH_SCORE = OVER_FINISH * 3,
                       SLOTS_PROGRESSED_SCORE = SLOTS_PROGRESSED * 0.05 * input_turn_id
                       #SOF_NEW_SCORE = (SLOTS_OVER_FINISH_NEW - SLOTS_OVER_FINISH  )  * 0.1 * norm_card_share ^ (1 / 2),
                       #TTF_NEW_SCORE = (TURNS_TO_FINISH - 1 - TURNS_TO_FINISH_NEW)  * 0.5 * norm_card_share ^ (1 / 2),
                       #NEXT_EXHAUST_SCORE = 0.5 * (((NEW_MOVE_ORDER - 1) / total_cyclers) ^ (1.5) * NEXT_EXHAUST * pmax(min_ttf - 5 - FINISH_RANK, 1) ^ 1.5 * -0.15  +
                       #                               IS_ROULER * ((NEW_MOVE_ORDER - 1) / total_cyclers) ^ (1.5) * NEXT_EXHAUST * pmax(min_ttf - 5 - FINISH_RANK, 1) ^ 1.5 * -0.15 * -0.25),
                       # NEXT_MOVE_DIFF_SCORE = NEXT_MOVE_DIFF * 0.05 * pmax(min_ttf - 5, 0.1) - IS_ROULER * NEXT_MOVE_DIFF * 0.02 * pmax(min_ttf - 5, 0.1)
  )]

  scoring_data[, ':=' (MOVE_DIFF_SCORE = MOVE_DIFF_SCORE - min(MOVE_DIFF_SCORE),
                       EXHAUST_SCORE = EXHAUST_SCORE - min(EXHAUST_SCORE),
                       #   TTF_SCORE = RELATIVE_TTF * 20 * ((total_cyclers - max(MOVE_ORDER, 4)) / total_cyclers),
                       TTF_SCORE  = TTF_SCORE - min(TTF_SCORE),
                       SOF_SCORE = SOF_SCORE - min(SOF_SCORE),
                       FINISH_RANK_SCORE = FINISH_RANK_SCORE - min(FINISH_RANK_SCORE),
                       # CYC_DIST_SCORE = DIST_TO_TEAM * - 0.03 * pmax(TURNS_TO_FINISH - 3, 0),
                       MOVE_ORDER_SCORE  = MOVE_ORDER_SCORE - min(MOVE_ORDER_SCORE),
                       OVER_FINISH_SCORE  = OVER_FINISH_SCORE - min(OVER_FINISH_SCORE),
                       SLOTS_PROGRESSED_SCORE  = SLOTS_PROGRESSED_SCORE - min(SLOTS_PROGRESSED_SCORE)
                       #SOF_NEW_SCORE  = SOF_NEW_SCORE - min(SOF_NEW_SCORE),
                       #TTF_NEW_SCORE = TTF_NEW_SCORE - min(TTF_NEW_SCORE),
                       #NEXT_EXHAUST_SCORE = NEXT_EXHAUST_SCORE - min(NEXT_EXHAUST_SCORE),
                       #NEXT_MOVE_DIFF_SCORE = NEXT_MOVE_DIFF_SCORE - min(NEXT_MOVE_DIFF_SCORE)
  ), by = CYCLER_ID]




  scoring_data[, TOT_SCORE_MINE := (MOVE_DIFF_SCORE +
                                      EXHAUST_SCORE +
                                      SOF_SCORE +
                                      TTF_SCORE +
                                      #   CYC_DIST_SCORE +
                                      FINISH_RANK_SCORE +
                                      SLOTS_PROGRESSED_SCORE +
                                      MOVE_ORDER_SCORE +
                                      OVER_FINISH
                                    #              SOF_NEW_SCORE +
                                    #             TTF_NEW_SCORE
                                    #               NEXT_EXHAUST_SCORE +
                                    #              NEXT_MOVE_DIFF_SCORE


  )]


  # scoring_data[, TOT_NEXT_SCORE := (
  #   (SOF_NEW_SCORE +
  #      TTF_NEW_SCORE +
  #      NEXT_EXHAUST_SCORE +
  #      NEXT_MOVE_DIFF_SCORE)
  # )]

  #scoring_data[, TOT_NEXT_SCORE := TOT_NEXT_SCORE - min(TOT_NEXT_SCORE)]
  scoring_data[, TEAM_CYCLERS_LEFT := .N, by = .(case_id, TEAM_ID)]
  scoring_data[, TEAM_SCORE_AVG := mean(TOT_SCORE_MINE), by = .(case_id, TEAM_ID)]

  scoring_data[, TOT_CYCLERS_LEFT := .N, by = .(case_id)]
  scoring_data[, OTHER_CYCLERS_LEFT := TOT_CYCLERS_LEFT - TEAM_CYCLERS_LEFT, by = .(case_id)]
  scoring_data[, SUM_OF_OTHER_CYCLER_SCORE := sum(TOT_SCORE_MINE) - TEAM_SCORE_AVG * TEAM_CYCLERS_LEFT , by = .(case_id)]

  scoring_data[, OTHER_AVG_SCORE := SUM_OF_OTHER_CYCLER_SCORE / OTHER_CYCLERS_LEFT]
  scoring_data[, OTHER_AVG_SCORE := ifelse(is.finite(OTHER_AVG_SCORE), OTHER_AVG_SCORE, 0)]

  #  scoring_data[, MY_RELATIVE_SCORE := ]
  scoring_data[, TOT_SCORE :=  TOT_SCORE_MINE]
  my_data <- scoring_data[TEAM_ID == bot_team_id]
  my_data[, SAFE_CYCLER_MEAN_TTF := CYCLER_MEAN_TTF + 2]
  my_data[, CYCLER_WEIGHT := (1 - (SAFE_CYCLER_MEAN_TTF) / (sum(SAFE_CYCLER_MEAN_TTF) + 0.01)), by = .(TEAM_ID, case_id)]
  my_data[, CYCLER_WEIGHT := ifelse(CYCLER_WEIGHT == 0, 1, CYCLER_WEIGHT)]

  check_res <- my_data[, .(TEAM_SCORE = sum(TOT_SCORE * CYCLER_WEIGHT),
                           MOVE_DIFF_SCORE  = sum(MOVE_DIFF_SCORE * CYCLER_WEIGHT),
                           EXHAUST_SCORE = sum(EXHAUST_SCORE * CYCLER_WEIGHT),
                           SLOTS_PROGRESSED_SCORE = sum(SLOTS_PROGRESSED_SCORE * CYCLER_WEIGHT),
                           TTF_SCORE = sum(TTF_SCORE * CYCLER_WEIGHT),
                           TURNS_TO_FINISH = sum(TURNS_TO_FINISH * CYCLER_WEIGHT),
                           SOF_SCORE = sum(SOF_SCORE * CYCLER_WEIGHT),
                           FINISH_RANK_SCORE = sum(FINISH_RANK_SCORE * CYCLER_WEIGHT),

                           #     CYC_DIST_SCORE = sum(CYC_DIST_SCORE),
                           MOVE_ORDER_SCORE = sum(MOVE_ORDER_SCORE * CYCLER_WEIGHT),
                           OVER_FINISH_SCORE = sum(OVER_FINISH_SCORE * CYCLER_WEIGHT)
                           #NEXT_SCORE = sum(TOT_NEXT_SCORE * CYCLER_WEIGHT)
  ), by = .(MOVES, TEAM_ID, case_id, PROB_PRODUCT, CYCLERS, OPPONENT_PROB)]

  # check_res[, OPPONENT_CASE_PROB := prod(PROB_PRODUCT) / PROB_PRODUCT, by = case_id]
  # check_res[, OPPONENT_CASE_PROB := ifelse(is.na(OPPONENT_CASE_PROB), 0.000001, OPPONENT_CASE_PROB)]
  #

  check_res2 <- check_res[, .(EV = sum(TEAM_SCORE * OPPONENT_PROB) / sum(OPPONENT_PROB),
                              MOVE_DIFF_SCORE  = sum(MOVE_DIFF_SCORE * OPPONENT_PROB) / sum(OPPONENT_PROB),
                              EXHAUST_SCORE = sum(EXHAUST_SCORE * OPPONENT_PROB) / sum(OPPONENT_PROB),
                              SLOTS_PROGRESSED_SCORE = sum(SLOTS_PROGRESSED_SCORE * OPPONENT_PROB) / sum(OPPONENT_PROB),
                              TTF_SCORE = sum(TTF_SCORE * OPPONENT_PROB) / sum(OPPONENT_PROB),
                              TURNS_TO_FINISH = sum(TURNS_TO_FINISH * OPPONENT_PROB) / sum(OPPONENT_PROB),
                              FINISH_RANK_SCORE = sum(FINISH_RANK_SCORE * OPPONENT_PROB) / sum(OPPONENT_PROB),
                              SOF_SCORE = sum(SOF_SCORE * OPPONENT_PROB) / sum(OPPONENT_PROB),
                              #   CYC_DIST_SCORE = mean(CYC_DIST_SCORE),
                              MOVE_ORDER_SCORE = sum(MOVE_ORDER_SCORE * OPPONENT_PROB) / sum(OPPONENT_PROB),
                              OVER_FINISH_SCORE = sum(OVER_FINISH_SCORE * OPPONENT_PROB) / sum(OPPONENT_PROB)
                              #   NEXT_SCORE = sum(NEXT_SCORE * OPPONENT_PROB) / sum(OPPONENT_PROB)
  ), by = .(CYCLERS, MOVES, TEAM_ID)][order(-EV)]

  # check_res2[, ':=' (MOVE_DIFF_SCORE = MOVE_DIFF_SCORE - min(MOVE_DIFF_SCORE),
  #                    EXHAUST_SCORE = EXHAUST_SCORE - min(EXHAUST_SCORE),
  #                    #   TTF_SCORE = RELATIVE_TTF * 20 * ((total_cyclers - max(MOVE_ORDER, 4)) / total_cyclers),
  #                    TTF_SCORE  = TTF_SCORE - min(TTF_SCORE),
  #                    SOF_SCORE = SOF_SCORE - min(SOF_SCORE),
  #                    FINISH_RANK_SCORE = FINISH_RANK_SCORE - min(FINISH_RANK_SCORE),
  #                    # CYC_DIST_SCORE = DIST_TO_TEAM * - 0.03 * pmax(TURNS_TO_FINISH - 3, 0),
  #                    MOVE_ORDER_SCORE  = MOVE_ORDER_SCORE - min(MOVE_ORDER_SCORE),
  #                    OVER_FINISH_SCORE  = OVER_FINISH_SCORE - min(OVER_FINISH_SCORE),
  #                    SLOTS_PROGRESSED_SCORE  = SLOTS_PROGRESSED_SCORE - min(SLOTS_PROGRESSED_SCORE)

  # NEXT_SCORE = NEXT_SCORE - min(NEXT_SCORE)
  #)]
  # check_res2[, ROW_SUM := MOVE_DIFF_SCORE + EXHAUST_SCORE + TTF_SCORE + SOF_SCORE + FINISH_RANK_SCORE +
  #              MOVE_ORDER_SCORE + OVER_FINISH_SCORE + SLOTS_PROGRESSED_SCORE ]
  # check_res2[, max_score := abs(MOVE_DIFF_SCORE)+ abs(EXHAUST_SCORE)+ abs(SLOTS_PROGRESSED_SCORE)+ abs(TTF_SCORE)+ abs(FINISH_RANK_SCORE)+
  #              abs(SOF_SCORE)+ abs(MOVE_ORDER_SCORE)+ abs(OVER_FINISH_SCORE)]
  # check_res2[, ':=' (MOVE_DIFF_SCORE = MOVE_DIFF_SCORE / max_score,
  #                    EXHAUST_SCORE = EXHAUST_SCORE / max_score,
  #                    SLOTS_PROGRESSED_SCORE = SLOTS_PROGRESSED_SCORE / max_score,
  #                    TTF_SCORE = TTF_SCORE / max_score,
  #                    FINISH_RANK_SCORE = FINISH_RANK_SCORE / max_score,
  #                    SOF_SCORE = SOF_SCORE / max_score,
  #                    MOVE_ORDER_SCORE = MOVE_ORDER_SCORE / max_score,
  #                    OVER_FINISH_SCORE = OVER_FINISH_SCORE / max_score
  # )]
  # agggr <-suppressWarnings(melt.data.table(check_res2[, .(MOVE_DIFF_SCORE = mean(MOVE_DIFF_SCORE),
  #                                                         EXHAUST_SCORE    = mean(EXHAUST_SCORE   ),
  #                                                         SLOTS_PROGRESSED_SCORE    = mean(SLOTS_PROGRESSED_SCORE   ),
  #                                                         TTF_SCORE = mean(TTF_SCORE),
  #                                                         FINISH_RANK_SCORE    = mean(FINISH_RANK_SCORE   ),
  #                                                         SOF_SCORE = mean(SOF_SCORE),
  #                                                         MOVE_ORDER_SCORE  = mean(MOVE_ORDER_SCORE ),
  #                                                         OVER_FINISH_SCORE    = mean(OVER_FINISH_SCORE   )
  #                     )]))  [order(-value)]


  #monitor <<- agggr
  #print(agggr)
  #check_res2[order(-ROW_SUM)]
  # check_res2[, max_score := pmax(abs(MOVE_DIFF_SCORE), abs(EXHAUST_SCORE), abs(SLOTS_PROGRESSED_SCORE), abs(TTF_SCORE), abs(FINISH_RANK_SCORE),
  #                                abs(SOF_SCORE), abs(MOVE_ORDER_SCORE), abs(OVER_FINISH_SCORE), abs(NEXT_SCORE))]
  #
  # check_res2[, ':=' (MOVE_DIFF_SCORE = MOVE_DIFF_SCORE / max_score,
  #                    EXHAUST_SCORE = EXHAUST_SCORE / max_score,
  #                    SLOTS_PROGRESSED_SCORE = SLOTS_PROGRESSED_SCORE / max_score,
  #                    TTF_SCORE = TTF_SCORE / max_score,
  #                    FINISH_RANK_SCORE = FINISH_RANK_SCORE / max_score,
  #                    SOF_SCORE = SOF_SCORE / max_score,
  #                    MOVE_ORDER_SCORE = MOVE_ORDER_SCORE / max_score,
  #                    OVER_FINISH_SCORE = OVER_FINISH_SCORE / max_score,
  #                    NEXT_SCORE = NEXT_SCORE / max_score)]

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

