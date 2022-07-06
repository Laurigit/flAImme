# track <- 2
# cyclers <- c(4,1,3,6,2,5)
# game_status <- start_game(cyclers,track, STG_TRACK_PIECE, STG_TRACK)
# ijk <- ijk_map(track, STG_TRACK_PIECE, STG_TRACK)

#calculate_mixed_strategy
calculate_mixed_strategy <- function(combinations_data_input, consensus_config_id = ADM_CONSENSUS_CONFIG, deck_status) {

  #combinations_data_input <- combinations_output
  join_track_left <- combinations_data_input
      #join_track_left[, TTF_RELATIVE := min(TURNS_TO_FINISH) - TURNS_TO_FINISH, by = case_id]


     # join_track_left[, MOVE_ORDER := seq_len(.N), by = case_id]
      setkeyv(join_track_left, cols = c("case_id", "TEAM_ID", "CYCLER_ID"))
        #PELKÄSTÄ LIIKKUMISESTA PITÄÄ ANTAA PISTEITÄ MYÖS, vähintääb tie breakeraita varten, jos sama ev muuten
     # join_track_left[, MOVE_DIFF_RELATIVE := MOVE_DIFF - (sum(MOVE_DIFF) - MOVE_DIFF) / (.N - 1), by = case_id]
      cycler_count <- join_track_left[, .N, by = CYCLER_ID][, .N]
      min_ttf <- join_track_left[, min(TURNS_TO_FINISH)] + 0.01
      mean_ttf <- join_track_left[, mean(TURNS_TO_FINISH)] + 0.01

      join_track_left[, ':=' (MOVE_DIFF_SCORE = MOVE_DIFF,
                              EXHAUST_SCORE = -1 * EXHAUST * (MOVE_ORDER / cycler_count) * 2 / (min_ttf / 8),
                            #  TTF_SCORE = RELATIVE_TTF * 20 * ((total_cyclers - max(MOVE_ORDER, 4)) / total_cyclers),
                             TTF_SCORE = (mean_ttf - TURNS_TO_FINISH + SLOTS_OVER_FINISH / 6) * (3 / min_ttf),
                             # CYC_DIST_SCORE = DIST_TO_TEAM * - 0.03 * pmax(TURNS_TO_FINISH - 3, 0),
                              MOVE_ORDER_SCORE = -1 * MOVE_ORDER / cycler_count,
                              OVER_FINISH_SCORE = OVER_FINISH * 3, #NOT MAKE TUU HIGH
                            FINISH_RANK_SCORE = FINISH_RANK * (6 / min_ttf),
                             SLOTS_PROGRESSED_SCORE = SLOTS_PROGRESSED * 0.5 / min_ttf)]

      join_track_left[, TOT_SCORE := (MOVE_DIFF_SCORE +
                                        EXHAUST_SCORE +
                                        TTF_SCORE +
                                     #   CYC_DIST_SCORE +
                                        MOVE_ORDER_SCORE +
                                        OVER_FINISH_SCORE+
                                       FINISH_RANK_SCORE +
                                       SLOTS_PROGRESSED_SCORE)]
      #join_track_left[, all_moves := list((list(MOVEMENT))), by = case_id]
      #join_track_left[,  opponent_moves := create_other_moves(all_moves, CYCLER_ID), by = .(case_id, TEAM_ID)]



     # join_track_left[, MIX_STRATEGY_CAP := prod(ODDS_OF_DRAWING_MOVEMENT), by = .(case_id, TEAM_ID)]



      #ODDS_OF_DRAWING_CASE_MOVEMENTS_BY_TEAM = capped mixed strategy weight


      #calculate new odds based on expect moves of others


     # browser()
      check_res <- join_track_left[, .(TEAM_SCORE = sum(TOT_SCORE),
                                       MOVE_DIFF_SCORE  = sum(MOVE_DIFF_SCORE),
                                       EXHAUST_SCORE = sum(EXHAUST_SCORE),
                                       TTF_SCORE = sum(TTF_SCORE),
                                      TURNS_TO_FINISH = min(TURNS_TO_FINISH),
                                  #     CYC_DIST_SCORE = sum(CYC_DIST_SCORE),
                                       MOVE_ORDER_SCORE = sum(MOVE_ORDER_SCORE),
                                       OVER_FINISH_SCORE = sum(OVER_FINISH_SCORE)), by = .(TEAM_ID, case_id, MOVES,
                                                                                           CYCLERS)]

      check_res2 <- check_res[, .(EV = mean(TEAM_SCORE),
                                  MOVE_DIFF_SCORE  = mean(MOVE_DIFF_SCORE),
                                  EXHAUST_SCORE = mean(EXHAUST_SCORE),
                                  TTF_SCORE = mean(TTF_SCORE),
                                  TURNS_TO_FINISH = mean(TURNS_TO_FINISH),
                               #   CYC_DIST_SCORE = mean(CYC_DIST_SCORE),
                                  MOVE_ORDER_SCORE = mean(MOVE_ORDER_SCORE),
                                  OVER_FINISH_SCORE = mean(OVER_FINISH_SCORE)
                                  ), by = .(CYCLERS, MOVES, TEAM_ID)][order(-EV)]
      check_res2[, PROB_PRODUCT := 0.1]
      check_res2[, M1 := as.numeric(str_sub(MOVES, 1, 1))]
      check_res2[, M2 := as.numeric(str_sub(MOVES, 3, 3))]
      check_res2[, C1 := as.numeric(word(CYCLERS, 1, 1, sep = fixed("_")))]
      check_res2[, C2 := as.numeric(word(CYCLERS, 2, 2, sep = fixed("_")))]
      check_res2[, PENALTY_1 := 0]
      check_res2[, PENALTY_2 := 0]
      cap_data_all <- copy(deck_status)
      cap_data_all[, count_me := ifelse(Zone != "Removed", 1, 0)]
      cap_data <- cap_data_all[, .(count = sum(count_me)), by = .(CYCLER_ID, MOVEMENT)]

      cap_data[, mixed_cap := 1 - suppressWarnings(phyper(0,  count, sum( count) -  count, 4)), by = .(CYCLER_ID)]
      cap_data[, mixed_cap := ifelse(is.na(mixed_cap) & count == 0, 0, mixed_cap)]
      cap_data[, mixed_cap := ifelse(is.na(mixed_cap) & count >0, 1, mixed_cap)]

      #cap_data[, OTHER_MOVE := 8]
      # aggr_card_count <- deck_status[Zone != "Removed", .(count = .N), by = .(CYCLER_ID, MOVEMENT)]
      # join_count1 <- aggr_card_count[check_res2, on = .(MOVEMENT == M1, CYCLER_ID == C1)]#[order(TEAM_ID, - new_prob_uncapped)]
      # setnames(join_count1, c("CYCLER_ID", "MOVEMENT", "count"), c("C1", "M1", "card_count_C1"))
      # join_count2 <- aggr_card_count[join_count1, on = .(MOVEMENT == M2, CYCLER_ID == C2)]#[order(TEAM_ID, - new_prob_uncapped)]
      # setnames(join_count2, c("CYCLER_ID", "MOVEMENT", "count"), c("C2", "M2", "card_count_C2"))
      # join_count2[, mixed_cap_C1 := 1 - suppressWarnings(phyper(0, card_count_C1, sum(card_count_C1) - card_count_C1, 4)), by = .(C1, M1)]
      # join_count2[, mixed_cap_C2 := 1 - suppressWarnings(phyper(0, card_count_C2, sum(card_count_C2) - card_count_C2, 4)), by =.(C2, M2)]

      #static_model <- constrained_mixed_strategy_long_static(cap_data)
      #calculate which cycler to move first
      #calculate pick-order 1
      #calculate prick order 2
      #calculate combined mix_strat_cap


      #estimate combinations

      combs <- deck_status[Zone != "Removed", .N, .(CYCLER_ID, MOVEMENT)][, .N]

      iteration_limit <- max(min(50, ceiling(400 / combs)), 5)
      #gamma goes down first and then starts increasing. At which iteration, we are on bottom?
      bottom_iteration <- 4
      var_threshold <- 0.01
      #gamma level
      input_gamma <- 0.75
      #gamma delta coefficient. The sin formula varies from -1 to 1. Ending to 1. Going to -1 on the bottom iteration
      gamma_addition <- 0.4

      gamma_step <- pi/(iteration_limit - bottom_iteration)

      first_round_gamma <- -sin(pi / 2 - gamma_step * bottom_iteration) * gamma_addition + input_gamma

      prev_result <- calulate_mixed_strategy_inner_capped(check_res2, join_track_left, static_model = NULL, ADM_CYCLER_INFO, first_round_gamma, cap_data)
      #this is initializing data that tracks which combinations are still inthe simulation. Initially all
      remove_moves <- prev_result[, .(CYCLERS, MOVES, remove_me_if_na = FALSE)][1 == 0]
      drop_cases <- join_track_left[, .(case_id, remove_if_na = TRUE)]
      #initialize variable. Not a parameter

      prev_max_prob_diff <- 1
      continue <- 1
      iter_loop_count <- 0
      no_converge_counter <- 0
      add_more_gamma <- 0
      ##########

      discard_threshold <- 0.005
      remove_max_moves_per_iter_per_team <- 0

      while (continue == 1) {

        iter_loop_count <- iter_loop_count + 1
       # print("iter")
       # print(iter_loop_count)
        #always change gamma initially
        if (iter_loop_count <= bottom_iteration) {
        total_gamma <- -sin(pi / 2 - gamma_step * bottom_iteration + gamma_step * iter_loop_count) * gamma_addition + input_gamma
        #then only if no converge
        } else if (      add_more_gamma == 1) {
          total_gamma <- -sin(pi / 2 - gamma_step * bottom_iteration + gamma_step * (bottom_iteration + no_converge_counter)) * gamma_addition + input_gamma
          add_more_gamma <- 0
        }
   #  print("gamma")
   #   print(total_gamma)
     if (total_gamma > 0.2) {
      drop_cases_all_rows <- remove_moves[join_track_left, on = .(MOVES, CYCLERS)][!is.na(remove_me_if_na)]
      if (nrow(drop_cases_all_rows) > 0) {

        drop_cases <- drop_cases_all_rows[, .( remove_if_na = .N), by = .(case_id)]
         join_track_left <- drop_cases[join_track_left, on = .(case_id)][is.na(remove_if_na)]

        join_track_left[, remove_if_na := NULL]
      }
     }



      new_result <- calulate_mixed_strategy_inner_capped(prev_result, join_track_left, static_model = NULL, ADM_CYCLER_INFO, total_gamma, cap_data)


      setnames(prev_result, "PROB_PRODUCT", "PREV_PROB_PRODUCT")
      joinaa <- prev_result[new_result, on = .(CYCLERS, MOVES, TEAM_ID)]
      joinaa[, prob_diff := PROB_PRODUCT - PREV_PROB_PRODUCT]
      setorder(joinaa, TEAM_ID, PROB_PRODUCT)
      joinaa[, cum_prob := cumsum(PROB_PRODUCT), by = TEAM_ID]
      setorder(joinaa, TEAM_ID, PREV_PROB_PRODUCT)
      joinaa[, cum_prob_prev := cumsum(PREV_PROB_PRODUCT), by = TEAM_ID]
      joinaa[, ':=' (current_less_than_threshold = cum_prob > discard_threshold,
                     prev_less_than_threshold = cum_prob_prev > discard_threshold)]
      setkey(joinaa, PROB_PRODUCT)
      #remove only max n per iteration
      n_bottom <- joinaa[, head(.SD, remove_max_moves_per_iter_per_team), by = TEAM_ID]
      remove_moves <- n_bottom[!(current_less_than_threshold | prev_less_than_threshold), .(CYCLERS, MOVES, remove_me_if_na = FALSE)]
    #  print("REMOVING")
     # print(remove_moves)
      setkey(joinaa, prob_diff)
      #top 4 biggest changes by team
      ress <- joinaa[, tail(.SD, 4), by = TEAM_ID]
   #   print(ress)
      #max change in prob
   varia <-  ress[, max(prob_diff)]
  #  print(varia)

      if (varia > prev_max_prob_diff & iter_loop_count > bottom_iteration) {
        no_converge_counter <- no_converge_counter + 1
        add_more_gamma <- 1
      }
      prev_result <- new_result
      prev_max_prob_diff <- varia

      if (varia < var_threshold | iter_loop_count >= iteration_limit) {
        continue <- 0
      }
      }
     # new_result[order(- EV)][1:30]

      result_total <- NULL


      result_total$opponent_strat <- new_result

ss_probs <- new_result[, .(MOVES, CYCLERS, TEAM_ID, PROB_PRODUCT)]

join_probs_to_combinations <- ss_probs[join_track_left, on = .(MOVES, CYCLERS, TEAM_ID)]
join_probs_to_combinations[, CYCLER_PER_TEAM := .N, by = .(TEAM_ID, case_id)]
#nämä laskut validoitu, että ok
join_probs_to_combinations[, CASE_PROB := prod(PROB_PRODUCT ^ (1/CYCLER_PER_TEAM)), by = case_id]
join_probs_to_combinations[, OPPONENT_PROB := CASE_PROB / (PROB_PRODUCT)]

      setorder(join_probs_to_combinations, case_id, -NEW_SQUARE)
      result_total$combinations <- join_probs_to_combinations
    #  browser()
      if (nrow(new_result[is.na(PROB_PRODUCT)]) > 0) {
      browser()
      }
      return(result_total)




}
