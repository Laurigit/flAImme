#calculcate move distribution of everyone

calc_move_range_phase_2 <- function(game_status, deck_status, ctM_data, p2_score) {
  #if N = 0 it has been calculated because of blocking or slipstream
  options <- ctM_data[N > 0]

  used_game_status <- copy(game_status)

  #card pool by cycler_id
  pool_data <- deck_status[, .(move_list = list(MOVEMENT)), by = CYCLER_ID]

  aggr_to_move_cycler <- deck_status[,. (.N), by = .(MOVEMENT, CYCLER_ID)]
  sscols_ctM <- options[, .(MOVEMENT, CYCLER_ID, turns_to_finish)]
  join_new_N <- sscols_ctM[aggr_to_move_cycler, on = .(CYCLER_ID, MOVEMENT)]
  #in case there are adjustments due to blocking, restircted option on ctm_Data based on deck_status


  #p2_score <-p2ScoreOutout

  #if only one is best, then calc odds of drawing it.
  #priority, turns_to_finish, highest movement

  join_new_N[, deck_size := sum(N), by = CYCLER_ID]

  #join_p2_score
  #same time removing cyclers that have moved
  joinp2_all <- p2_score[join_new_N, on = .(CYCLER_ID, MOVEMENT)]
  #use the non moving ones later
  non_moving <- joinp2_all[!CYCLER_ID %in% p2_score[, CYCLER_ID],. (MOVEMENT = 0, shared_odds = 1, turns_to_finish = NA, prio_group = 100), by = CYCLER_ID]
  #get still current pos of them
  non_moving_pos <- game_status[!CYCLER_ID %in% p2_score[, CYCLER_ID] & CYCLER_ID > 0, .(new_slot = GAME_SLOT_ID, CYCLER_ID)]
  #join_pos
  non_moving_with_pos <- non_moving_pos[non_moving, on = "CYCLER_ID"]

  #continue with the moving
  joinp2 <- joinp2_all[!is.na(score)]
  sorted <-  joinp2[order(CYCLER_ID, turns_to_finish, -score, -MOVEMENT )]
  sorted[, rowi := seq_len(.N)]
  temp_aggr <- sorted[, .N, by = .(CYCLER_ID, turns_to_finish, score, MOVEMENT)]
  temp_aggr[, prio_group := seq_len(.N), by = .(CYCLER_ID)][, N := NULL]
  join_prio <- temp_aggr[sorted, on = .(CYCLER_ID, turns_to_finish, score, MOVEMENT)]

  #aggregate_to_prio_group
  prio_aggr <- join_prio[, .(moves_accepted = list(MOVEMENT)), by = .(prio_group, CYCLER_ID)]
  #another copy for self join
  prio_aggr_self <- join_prio[, .(rejected_move = list(MOVEMENT)), by = .(prio_group = prio_group + 1, CYCLER_ID)]
  #another copy for self join
  prio_aggr_self_2nd <- join_prio[, .(rejected_move_2nd = list(MOVEMENT)), by = .(prio_group = prio_group + 2, CYCLER_ID)]
  #one more
  prio_aggr_self_3nd <- join_prio[, .(rejected_move_3nd = list(MOVEMENT)), by = .(prio_group = prio_group + 3, CYCLER_ID)]
  #self join
  self_join_prio_agg <- merge(x = prio_aggr, y = prio_aggr_self, by = c("CYCLER_ID", "prio_group"), all = TRUE, allow.cartesian = TRUE)
  #and next
  self_join_prio_agg_2nd <- merge(x = self_join_prio_agg, y = prio_aggr_self_2nd, by = c("CYCLER_ID", "prio_group"), all = TRUE, allow.cartesian = TRUE)
  #and one
  self_join_prio_agg_3nd <- merge(x = self_join_prio_agg_2nd, y = prio_aggr_self_3nd, by = c("CYCLER_ID", "prio_group"), all = TRUE, allow.cartesian = TRUE)
  #make it prettier
  self_join_prio_agg_3nd[, row_ident := seq_len(.N)]
  need_accpeted <- self_join_prio_agg_3nd[lengths(moves_accepted) > 0]


  #join pool
  pool_to_self_join <- pool_data[need_accpeted, on = .(CYCLER_ID)]
  pool_to_self_join[, odds := exact_draw_odds_calculator(move_list[[1]], moves_accepted[[1]], c(rejected_move[[1]], rejected_move_2nd[[1]], rejected_move_3nd[[1]])), by = .(row_ident)]
  pool_to_self_join[, odds := ifelse(is.na(odds), 0, odds)]
  #divide odds by accepted moves
  res_data <- pool_to_self_join[, .(CYCLER_ID, prio_group, shared_odds = odds / lengths(moves_accepted))]
  #join back
  join_odds_to_moves <- res_data[join_prio, on = .(CYCLER_ID, prio_group)]
  result_cols <- join_odds_to_moves[, .(CYCLER_ID, MOVEMENT, shared_odds, turns_to_finish, prio_group)]

  calc_slot <- result_cols[, .(new_slot = move_cycler(used_game_status, CYCLER_ID, MOVEMENT, ignore_block = TRUE, return_numeric_position = TRUE)), by = .(CYCLER_ID, MOVEMENT, shared_odds, turns_to_finish, prio_group)]
  #append non mobing
  all_data_res <- rbind(calc_slot, non_moving_with_pos)
  #aggr_to cyc_prio
  cyg_prio_agg <- all_data_res[, .(shared_odds = sum(shared_odds)), by = .(CYCLER_ID, prio_group)]
  cyg_prio_agg[, cumsum_prio_group := cumsum(shared_odds), by = .(CYCLER_ID)][,shared_odds := NULL]
  #join back
  join_aggr_prio <- cyg_prio_agg[all_data_res, on = .(CYCLER_ID, prio_group)]
  return(join_aggr_prio)
}


# options[, var(turns_to_finis), by = CYCLER_ID]
# zoom(game_status)
