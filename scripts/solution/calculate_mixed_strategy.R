# track <- 2
# cyclers <- c(4,1,3,6,2,5)
# game_status <- start_game(cyclers,track, STG_TRACK_PIECE, STG_TRACK)
# ijk <- ijk_map(track, STG_TRACK_PIECE, STG_TRACK)

#calculate_mixed_strategy
calculate_mixed_strategy <- function(game_status, deck_status, ijk, ADM_AI_CONF, ADM_OPTIMAL_MOVES, ctM_data, STG_TEAM, calc_ttf = TRUE, min_ttf = 10) {


  slots_squares <- as.matrix(game_status[, .(SQUARE_ID, GAME_SLOT_ID)])
  reverse_slots_squares <- slots_squares[nrow(slots_squares):1,]
  #


    range <- suppressWarnings(calc_move_range(game_status, deck_status, ctM_data, STG_CYCLER))

    cyclers <- game_status[CYCLER_ID > 0, CYCLER_ID]
    cycler_count <- length(cyclers)

    sscols <- range[, .(TEAM_ID, CYCLER_ID, MOVEMENT)]



    #sscols <- res_move[sscols_all, on = .(CYCLER_ID, MOVEMENT)][OPTION == TRUE | CYCLER_ID != moving_cycler]

    aggr <- sscols[, .N, by = CYCLER_ID]
    poss_combs <- prod(aggr[, N])
    cyclers <- sscols[, .N, by = CYCLER_ID][, CYCLER_ID]
    tot_data <- data.table(del_me = "")
    for (cyc_loop in cyclers) {
      tot_data <- CJ.dt(tot_data, sscols[CYCLER_ID == cyc_loop, .(CYCLER_ID, odds = 0,  MOVEMENT)])

    }
    tot_data[, del_me := NULL]
    appendloop <-  tot_data[, 1:3, with = FALSE]
    setnames(appendloop, colnames(appendloop), c("CYCLER_ID", "odds", "MOVEMENT"))
    appendloop[, case_id := seq_len(.N)]
    tot_data[, (1:3) := NULL]
    if (length(cyclers) > 1) {
    for (appendCOunt in 1:(length(cyclers) - 1)) {

      #tot_data[, del_me := NULL]
      splitcol <- tot_data[, 1:3, with = FALSE]
      setnames(splitcol, colnames(splitcol), c("CYCLER_ID", "odds", "MOVEMENT"))
      splitcol[, case_id := seq_len(.N)]

      appendloop <- rbind(appendloop, splitcol)
      tot_data[, (1:3) := NULL]
    }
    }
    filter_zero_all <- appendloop[order(case_id, CYCLER_ID)]
    cases <- filter_zero_all[, .SD[sample(.N, min(1000, .N))], by = .(CYCLER_ID, MOVEMENT)][, .N, by = case_id]
   # print(nrow(filter_zero_all))
    filter_zero <- filter_zero_all[case_id %in% cases[, case_id]]
   # print(nrow(filter_zero))
    filter_zero
    #join deck left

    cycler_movement <- range[, .(CYCLER_ID, MOVEMENT, DRAW_ODDS, DECK_LEFT)]

    join_deck_left <- cycler_movement[filter_zero, on = .(CYCLER_ID, MOVEMENT)]



    #curr pos
    currpos <- game_status[CYCLER_ID > 0, .(CYCLER_ID, GAME_SLOT_ID, SQUARE_ID)]
    #add_drow_odds here

    #calculate draw odds for smart cyclers
    #jionteam
    currpos_team <- STG_CYCLER[currpos, on = "CYCLER_ID"][, .(CYCLER_ID, TEAM_ID, curr_pos = GAME_SLOT_ID, curr_square = SQUARE_ID)]
    #join tosimul
    join_info_all <- currpos_team[join_deck_left, on = "CYCLER_ID"]


    #aggr_odds <- join_info[case_id == 1, .(case_odds = prod(odds), cyc_vec = list(phaseless_simulation(game_status, 5, 6, 3, STG_CYCLER, .SD, TRUE))), by = case_id, .SDcols = c("MOVEMENT", "CYCLER_ID", "curr_pos", "TEAM_ID")]
    # join_info_all[, case_odds := prod(odds), by = case_id]
    # join_info_all[, case_odds_ranking_total := frank(-case_odds, ties.method = c("min"))]
    join_info <- join_info_all#[order(-case_odds)]

    #join_info[, cycler_odds_rank := frank(-case_odds), by = .(CYCLER_ID, MOVEMENT)]
    #case_odds < 30

    max_new_cyc_pos <- join_info[, max(curr_pos, na.rm = TRUE)]


      #thinking_time <- 2
      setorder(join_info, case_id, -curr_square)

      matr_ijk <- as.matrix(ijk)
      join_info[, c("cyc_id_copy",
                    "MOVE_DIFF",
                    "NEW_GAME_SLOT_ID",
                    "EXHAUST",
                    "NEW_SQUARE") := as.data.table(move_cycler_c(as.matrix(.SD), matr_ijk, reverse_slots_squares)),
                by = .(case_id), .SDcols = c("CYCLER_ID", "MOVEMENT", "curr_pos")]


     # join_info <-

      pre_track <- precalc_track(game_status)
      track_ss <- pre_track$aggr_to_slots[, .(NEW_GAME_SLOT_ID = GAME_SLOT_ID, TRACK_LEFT)]

      join_track_left_without_CYCLERS_COL <- track_ss[join_info, on = .(NEW_GAME_SLOT_ID)]
      ss <- ADM_CYCLER_INFO[, .(CYCLER_ID,  CYCLERS = paste0(CYCLER_ID, collapse = "_")), by = TEAM_ID]
      join_track_left <- ss[join_track_left_without_CYCLERS_COL, on = .(TEAM_ID, CYCLER_ID)]

      setkeyv(join_track_left, cols = c("case_id",  "TEAM_ID", "CYCLER_ID"))
      if (calc_ttf == TRUE) {

      how_many_missing <- ADM_OPTIMAL_MOVES[join_track_left, on = .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS)]

      coverage <- how_many_missing[is.na(TURNS_TO_FINISH), .N, by = .(TRACK_LEFT, DECK_LEFT)][, .N] /
        how_many_missing[, .N, by = .(TRACK_LEFT, DECK_LEFT)][, .N]
      missing <- how_many_missing[is.na(TURNS_TO_FINISH), .N, by = .(TRACK_LEFT, DECK_LEFT)][, .N]
      print(missing)
      print(coverage)
      start_time <- Sys.time()
      join_track_left[!is.na(TRACK_LEFT), TURNS_TO_FINISH := as.double(finish_turns_db(con, TRACK_LEFT, DECK_LEFT, pre_track, NEW_GAME_SLOT_ID,
                                                                                       draw_odds_raw_data = DRAW_ODDS, save_to_DB = TRUE)),
                      by = .(NEW_GAME_SLOT_ID, TRACK_LEFT, DECK_LEFT, DRAW_ODDS)]
      time_diff <- difftime(Sys.time(), start_time, "seconds")
      print(time_diff)
      print(as.numeric(time_diff) / missing)
      } else {
        print("EITÄNNE")
        join_track_left[, TURNS_TO_FINISH := 10]
      }
      #scoora exhaust, move order, ttf, move_diff, squares over finish, ero tiimikaveriin

      #join_track_left[, negative_new_game_slot_id := ifelse(CYCLER_ID %% 2 == 0, NEW_GAME_SLOT_ID, - NEW_GAME_SLOT_ID)]


      #it's good to be close, but does not matter if the distance is really long. Formula is tested in excel.
      join_track_left[, ':=' (#DIST_TO_TEAM = 1 - 1 / (floor(abs(sum(negative_new_game_slot_id)) / 1.1) + 1.5) ^ 2,

                              MOVES = paste0(MOVEMENT, collapse = "_")) , by = .(case_id, TEAM_ID)]
     # join_track_left[, DIST_TO_TEAM2 := min(max(dist_to_team_uncapped, 1) - 1, 2), by = .(case_id, TEAM_ID, CYCLER_ID)]



      #FINISH SCORE
      finish_slot <- pre_track$aggr_to_slots[FINISH == 1, GAME_SLOT_ID]
      join_track_left[, OVER_FINISH := pmax(NEW_GAME_SLOT_ID - finish_slot, 0)]
      #setkeyv sorts cyclers by case_id and new_square
      setorder(join_track_left, case_id, -NEW_SQUARE)
      join_track_left[, ':=' (CYCLER_MEAN_TTF = mean(TURNS_TO_FINISH)), by = .(CYCLER_ID)]
      join_track_left[, ':=' (TTF_DIFF_OF_MEAN = CYCLER_MEAN_TTF - TURNS_TO_FINISH)]
      #  join_track_left[, ':=' (TEAM_TTF_MEAN = mean(TURNS_TO_FINISH), cycs = .N), by = .(case_id, TEAM_ID)]
      join_track_left[, ':=' (CASE_DIFF = sum(TTF_DIFF_OF_MEAN), tot_cycs = .N), by = .(case_id)]
      join_track_left[, ':=' (COMPETITOR_AVG_TTF = (CASE_DIFF - TTF_DIFF_OF_MEAN) / (tot_cycs - 1))]
      join_track_left[, ':=' (RELATIVE_TTF = (TTF_DIFF_OF_MEAN - COMPETITOR_AVG_TTF))]#positive is good
      join_track_left[, ':=' (SLOTS_PROGRESSED = NEW_GAME_SLOT_ID - curr_pos)]
      # join_track_left[, ':=' (TEAM_TTF = sum(TURNS_TO_FINISH), cycs = .N), by = .(case_id, TEAM_ID)]
      # join_track_left[, ':=' (TEAM_TTF_MEAN = mean(TURNS_TO_FINISH), cycs = .N), by = .(case_id, TEAM_ID)]
      # join_track_left[, ':=' (CASE_TTF = sum(TURNS_TO_FINISH), tot_cycs = .N), by = .(case_id)]
      # join_track_left[, ':=' (COMPETITOR_AVG_TTF = (CASE_TTF - TEAM_TTF) / (tot_cycs - cycs))]
      # join_track_left[, ':=' (RELATIVE_TTF = (COMPETITOR_AVG_TTF ^ (2/3) - (TEAM_TTF / cycs) ^ (2/3)))]
      join_track_left[, ':=' (
     # TTF_RELATIVE = quantile(TURNS_TO_FINISH, 0.35) - TURNS_TO_FINISH,
                              MOVE_ORDER = seq_len(.N),
                              MOVE_DIFF_RELATIVE = MOVE_DIFF - (sum(MOVE_DIFF) - MOVE_DIFF) / (.N - 1)
                              ), by = case_id]

      #join_track_left[, TTF_RELATIVE := min(TURNS_TO_FINISH) - TURNS_TO_FINISH, by = case_id]


     # join_track_left[, MOVE_ORDER := seq_len(.N), by = case_id]
      setkeyv(join_track_left, cols = c("case_id", "TEAM_ID", "CYCLER_ID"))
        #PELKÄSTÄ LIIKKUMISESTA PITÄÄ ANTAA PISTEITÄ MYÖS, vähintääb tie breakeraita varten, jos sama ev muuten
     # join_track_left[, MOVE_DIFF_RELATIVE := MOVE_DIFF - (sum(MOVE_DIFF) - MOVE_DIFF) / (.N - 1), by = case_id]

      join_track_left[, ':=' (MOVE_DIFF_SCORE = MOVE_DIFF_RELATIVE * 0.15 * pmax(min_ttf - 3, 0),
                              EXHAUST_SCORE = (1 + MOVE_ORDER / cycler_count) * EXHAUST * pmax(min_ttf - 3, 0) ^ 1.2 * -0.2,
                              TTF_SCORE = RELATIVE_TTF * 8 * ((total_cyclers - MOVE_ORDER + 1) / total_cyclers),
                             # CYC_DIST_SCORE = DIST_TO_TEAM * - 0.03 * pmax(TURNS_TO_FINISH - 3, 0),
                              MOVE_ORDER_SCORE = -MOVE_ORDER * 0.05 * (15 - min_ttf),
                              OVER_FINISH_SCORE = OVER_FINISH * 100,
                             SLOTS_PROGRESSED_SCORE = SLOTS_PROGRESSED * 0.005 * (16 - min_ttf))]

      join_track_left[, TOT_SCORE := (MOVE_DIFF_SCORE +
                                        EXHAUST_SCORE +
                                        TTF_SCORE +
                                     #   CYC_DIST_SCORE +
                                        MOVE_ORDER_SCORE +
                                        OVER_FINISH_SCORE+
                                       SLOTS_PROGRESSED_SCORE)]
      join_track_left[, all_moves := list((list(MOVEMENT))), by = case_id]
      #join_track_left[,  opponent_moves := create_other_moves(all_moves, CYCLER_ID), by = .(case_id, TEAM_ID)]

      #join_track_left[, ODDS_OF_DRAWING_MOVEMENT2 := calculate_odds_of_drawing_card(ORIG_DECK_LEFT, MOVEMENT)]
     # join_track_left[, ODDS_OF_DRAWING_MOVEMENT := calculate_odds_of_drawing_card(ORIG_DECK_LEFT, MOVEMENT), by = .(ORIG_DECK_LEFT, MOVEMENT)]


     # join_track_left[, MIX_STRATEGY_CAP := prod(ODDS_OF_DRAWING_MOVEMENT), by = .(case_id, TEAM_ID)]



      #ODDS_OF_DRAWING_CASE_MOVEMENTS_BY_TEAM = capped mixed strategy weight


      #calculate new odds based on expect moves of others



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
      check_res2[, C1 := as.numeric(str_sub(CYCLERS, 1, 1))]
      check_res2[, C2 := as.numeric(str_sub(CYCLERS, 3, 3))]
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

      static_model <- constrained_mixed_strategy_long_static(cap_data)
      #calculate which cycler to move first
      #calculate pick-order 1
      #calculate prick order 2
      #calculate combined mix_strat_cap




      iteration_limit <- 12
      #gamma goes down first and then starts increasing. At which iteration, we are on bottom?
      bottom_iteration <- 4
      var_threshold <- 0.01
      #gamma level
      input_gamma <- 0.75
      #gamma delta coefficient. The sin formula varies from -1 to 1. Ending to 1. Going to -1 on the bottom iteration
      gamma_addition <- 0.4

      gamma_step <- pi/(iteration_limit - bottom_iteration)

      first_round_gamma <- -sin(pi / 2 - gamma_step * bottom_iteration) * gamma_addition + input_gamma

      prev_result <- calulate_mixed_strategy_inner_capped(check_res2, join_track_left, static_model, ADM_CYCLER_INFO, first_round_gamma)
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



      new_result <- calulate_mixed_strategy_inner_capped(prev_result, join_track_left, static_model, ADM_CYCLER_INFO, total_gamma)


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
     # print(ress)
      #max change in prob
      varia <-  ress[, max(prob_diff)]
    # print(varia)

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
      result_total$combinations <- join_track_left
    #  browser()
      if (nrow(new_result[is.na(PROB_PRODUCT)]) > 0) {
      browser()
      }
      return(result_total)




}
