#calculcate move distribution of everyone

calc_move_range_phase_2 <- function(game_status, deck_status, ctM_data, p2_score) {
  #if N = 0 it has been calculated because of blocking or slipstream
  options <- ctM_data[N > 0]


#jäit siihen, että laitoit CTM dataan laskemaan vaan movementteja, eikä pelattuja kortteja.
#Sitten calc_move_range_phase2 luulee, että ei oo enää mahd

  used_game_status <- copy(game_status)

  aggr_to_move_cycler <- deck_status[,. (.N), by = .(MOVEMENT, CYCLER_ID)]
  sscols_ctM <- options[, .(MOVEMENT, CYCLER_ID, TURNS_TO_FINISH, actual_movement, new_slot_after_moving)]
  join_new_N <- sscols_ctM[aggr_to_move_cycler, on = .(CYCLER_ID, MOVEMENT)]
  #in case there are adjustments due to blocking, restircted option on ctm_Data based on deck_status


  #p2_score <-p2ScoreOutout

  #if only one is best, then calc odds of drawing it.
  #priority, TURNS_TO_FINISH, highest movement

  join_new_N[, deck_size := sum(N), by = CYCLER_ID]

  #join_p2_score
  #same time removing cyclers that have moved
  joinp2_all <- p2_score[join_new_N, on = .(CYCLER_ID, MOVEMENT)]
  #use the non moving ones later
  non_moving <- joinp2_all[!CYCLER_ID %in% p2_score[, CYCLER_ID],. (actual_movement = 0,new_slot_after_moving, MOVEMENT = 0, odds = 1, TURNS_TO_FINISH = NA, prio_group = 100), by = CYCLER_ID]
  #get still current pos of them
  #non_moving_pos <- game_status[!CYCLER_ID %in% p2_score[, CYCLER_ID] & CYCLER_ID > 0, .(new_slot = GAME_SLOT_ID, CYCLER_ID)]
  #join_pos
 # non_moving_with_pos <- non_moving_pos[non_moving, on = "CYCLER_ID"]

  #continue with the moving
  joinp2 <- joinp2_all[!is.na(score)]
  sorted <-  joinp2[order(CYCLER_ID, TURNS_TO_FINISH, -score, -actual_movement, MOVEMENT )]
  sorted[, rowi := seq_len(.N)]
  temp_aggr <- sorted[, .N, by = .(CYCLER_ID, TURNS_TO_FINISH, score, actual_movement, MOVEMENT)]
  temp_aggr[, prio_group := seq_len(.N), by = .(CYCLER_ID)][, N := NULL]
  join_prio <- temp_aggr[sorted, on = .(CYCLER_ID, TURNS_TO_FINISH, actual_movement, score, MOVEMENT)]
  join_prio[, prio_group_card_count := sum(N), by = .(prio_group, CYCLER_ID)]
  chosen_cols <- c("prio_group", "N")
  join_prio[, odds := melt(lapply(prio_group, exact_draw_odds_outer, .SD), id.vars = "CYCLER_ID"), by = CYCLER_ID, .SDcols = chosen_cols]
  #aggregate_to_prio_group

  calc_slot <- join_prio[, .(CYCLER_ID, MOVEMENT, odds, TURNS_TO_FINISH, prio_group, actual_movement, new_slot_after_moving)]

 #append non mobing
  all_data_res <- rbind(calc_slot, non_moving)
  #aggr_to cyc_prio
  cyg_prio_agg <- all_data_res[, .(shared_odds = sum(odds)), by = .(CYCLER_ID, prio_group)]
  cyg_prio_agg[, cumsum_prio_group := cumsum(shared_odds), by = .(CYCLER_ID)][,shared_odds := NULL]
  #join back
  join_aggr_prio <- cyg_prio_agg[all_data_res, on = .(CYCLER_ID, prio_group)]
  return(join_aggr_prio)
}


# options[, var(turns_to_finis), by = CYCLER_ID]
# zoom(game_status)
