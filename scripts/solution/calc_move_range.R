#calculcate move distribution of everyone

calc_move_range <- function(game_status, deck_status, ctM_data, STG_TEAM) {

  #input cTM

  #calc TTF based on played card
  #prioritise moves based on (TTF, actual_movement, -played_card)
  #calc played card p-distribution
  #return CARD, ODDS



  #if N = 0 it has been calculated because of blocking or slipstream
  options <- ctM_data[N > 0]
  used_game_status <- copy(game_status)
  #if only one is best, then calc odds of drawing it.
  #priority, turns_to_finish, highest movement'

  options[, deck_size := sum(N), by = CYCLER_ID]
  sorted <-  options[order(CYCLER_ID, TURNS_TO_FINISH, -actual_movement, MOVEMENT)]
  sorted[, rowi := seq_len(.N)]
  temp_aggr <- sorted[, .N, by = .(CYCLER_ID, TURNS_TO_FINISH, actual_movement, MOVEMENT)]
  temp_aggr[, prio_group := seq_len(.N), by = .(CYCLER_ID)][, N := NULL]
  join_prio <- temp_aggr[sorted, on = .(CYCLER_ID, TURNS_TO_FINISH, actual_movement, MOVEMENT)]
  join_prio[, prio_group_card_count := sum(N), by = .(prio_group, CYCLER_ID)]



  chosen_cols <- c("prio_group", "N")

  join_prio[, odds := melt(lapply(prio_group, exact_draw_odds_outer, .SD), id.vars = "CYCLER_ID"), by = CYCLER_ID, .SDcols = chosen_cols]

  # calc_slot <- join_prio[, .(new_slot = move_cycler(used_game_status,
  #                                                CYCLER_ID, MOVEMENT,
  #                                                ignore_block = TRUE,
  #                                                return_numeric_position = TRUE)), by = .(CYCLER_ID, MOVEMENT, odds, TURNS_TO_FINISH, actual_movement)]
   setnames(join_prio, "new_slot_after_moving", "new_slot")
   join_prio[, splitted_odds_no_scale := odds / 2 + 0.05]


  #join team to range
  range_joined_team <- STG_CYCLER[join_prio, on = "CYCLER_ID"]
  #scale odds to 1

  range_joined_team[, splitted_odds := splitted_odds_no_scale / sum(splitted_odds_no_scale), by = TEAM_ID]



  # zoom(used_game_status)
  #my random card
  range_joined_team[, row_id := seq_len(.N)]

  return(range_joined_team)
}


# options[, var(turns_to_finis), by = CYCLER_ID]
# zoom(game_status)
