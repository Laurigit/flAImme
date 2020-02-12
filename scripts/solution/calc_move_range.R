#calculcate move distribution of everyone

calc_move_range <- function(game_status, deck_status, ctM_data) {
  #if N = 0 it has been calculated because of blocking or slipstream
  options <- ctM_data[N > 0]
  used_game_status <- copy(game_status)
  #if only one is best, then calc odds of drawing it.
  #priority, turns_to_finish, highest movement
  options[, deck_size := sum(N), by = CYCLER_ID]
  sorted <-  options[order(CYCLER_ID, turns_to_finish, -MOVEMENT)]
  sorted[, rowi := seq_len(.N)]
  temp_aggr <- sorted[, .N, by = .(CYCLER_ID, turns_to_finish)]
  temp_aggr[, prio_group := seq_len(.N), by = .(CYCLER_ID)][, N := NULL]
  join_prio <- temp_aggr[sorted, on = .(CYCLER_ID, turns_to_finish)]
  join_prio[, prio_group_card_count := sum(N), by = .(prio_group, CYCLER_ID)]
  join_prio[prio_group == 1, odds := 1 - phyper(0, prio_group_card_count, deck_size - prio_group_card_count, 4), by = .(rowi)]
  aggr_odds <- join_prio[, .(prio_group = 2, Rest_of_odds = 1 - mean(odds, na.rm = TRUE)), by = CYCLER_ID]
  join_back_agg <- aggr_odds[join_prio, on = .(CYCLER_ID, prio_group)]
  #divide the share
  #rest of odds shared
  join_back_agg[, shared_odds := ifelse(!is.na(odds), odds * (N / prio_group_card_count), Rest_of_odds * (N / prio_group_card_count)), by = rowi]

  result <- join_back_agg[!is.na(shared_odds) & shared_odds > 0,. (CYCLER_ID, MOVEMENT, shared_odds, turns_to_finish)]

  calc_slot <- result[, .(new_slot = move_cycler(used_game_status, CYCLER_ID, MOVEMENT, ignore_block = TRUE, return_numeric_position = TRUE)), by = .(CYCLER_ID, MOVEMENT, shared_odds, turns_to_finish)]
  calc_slot[, splitted_odds := shared_odds / 2]
  return(calc_slot)
}


# options[, var(turns_to_finis), by = CYCLER_ID]
# zoom(game_status)
