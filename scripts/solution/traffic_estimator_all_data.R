traffic_estimator_all_data <- function(game_status, move_range, STG_CYCLER, cycler_id, phase_two_cyclers = NULL) {


  move_order <- game_status[CYCLER_ID > 0, .(CYCLER_ID, SQUARE_ID)][order(-SQUARE_ID)]
  move_order[, order := seq_len(.N)]
  relevant_cyclers <- move_order[order < move_order[CYCLER_ID == cycler_id, order], CYCLER_ID]
  #only care about those who move before me

  #cycler_id is the one considering moving and his teammate is not moving

  #move_range <- calc_slot
  cycler_posits <- game_status[CYCLER_ID > 0, .(posit = max(GAME_SLOT_ID)), by = CYCLER_ID]
  join_team <- STG_CYCLER[cycler_posits, on = "CYCLER_ID"]

  self_join <- merge(x = join_team, y = join_team, by = "TEAM_ID", allow.cartesian = TRUE)
  sscols_team_member_position <- self_join[CYCLER_ID.x != CYCLER_ID.y, .(TEAM_ID, CYCLER_ID = CYCLER_ID.x, teamMate_CYCLER_ID = CYCLER_ID.y, own_position = posit.x, tm_position = posit.y)]

  #who is my teamMate
  tm_CYCLER_ID <- sscols_team_member_position[TEAM_ID == sscols_team_member_position[CYCLER_ID == cycler_id, TEAM_ID] & CYCLER_ID != cycler_id, CYCLER_ID]
  #my team
  my_team <-  join_team[CYCLER_ID == cycler_id, TEAM_ID]

  #no move odds
#  no_move_odds <- join_team[, .(MOVEMENT = 0, shared_odds = 0  .5)]

  join_range_all <- sscols_team_member_position[move_range, on = "CYCLER_ID"]

  join_range <- join_range_all[TEAM_ID != my_team]# & CYCLER_ID %in% relevant_cyclers]

  if (!is.null(phase_two_cyclers)) {
    join_range <- join_range[CYCLER_ID %in% phase_two_cyclers]
  }


  #split odds by half. Chosee randoml who to move first
  join_range[, split_odds := shared_odds / 2]
  #join_range[CYCLER_ID %in% relevant_cyclers, relevant_for_blocking := TRUE]
  #aggregate_track_new_slot <- join_range[, .(count_possibilites = uniqueN(CYCLER_ID),
  #                                  sum_odds = sum(split_odds)), by = .(slot = new_slot, relevant_for_blocking)]
  #aggregate_track_new_slot[, slip_or_exh := TRUE]

  #aggregate_track_no_move <- join_range[, .(count_possibilites =  uniqueN(CYCLER_ID),
  #                                           sum_odds = sum(split_odds)), by = .(slot = tm_position, relevant_for_blocking)]
  #aggregate_track_no_move[, slip_or_exh := FALSE]
  #my teammate is not moving or phase 1 cyclers are not moving
#   if (!is.null(phase_two_cyclers)) {
#     tm_row <- join_team[!CYCLER_ID %in% phase_two_cyclers, .(slot = posit,
#                                                              count_possibilites = 1,
#                                                              sum_odds = 1,
#                                                              slip_or_exh = TRUE,
#                                                              relevant_for_blocking = TRUE)]
#   } else {
#     tm_row <- join_team[CYCLER_ID == tm_CYCLER_ID, .(slot = posit,
#                                                      count_possibilites = 1,
#                                                      sum_odds = 1,
#                                                      slip_or_exh = FALSE,
#                                                      relevant_for_blocking = TRUE)]
#   }
#
#
#
#   rbindaa <- rbind(aggregate_track_new_slot, aggregate_track_no_move, tm_row)
#
#
#
#   last_aggr <- rbindaa[, .(count_possibilites = sum(count_possibilites),
#                            sum_odds = sum(sum_odds)), by = .(slot, slip_or_exh, relevant_for_blocking)][order(slot)]
#   last_aggr[, CYCLER_ID := cycler_id]
#
#   #well, now we can join to original data
#  # cycler_range <- move_range[CYCLER_ID == cycler_id]
#   # join_result_to_range  <- last_aggr[move_range, on = .(CYCLER_ID == CYCLER_ID,   slot == new_slot), .(CYCLER_ID, MOVEMENT, sum_odds)]
#   # join_result_to_range[is.na(sum_odds), sum_odds := 0]
#   # join_result_to_range[CYCLER_ID %in% relevant_cyclers, relevant_for_blocking := TRUE]
#
# return(last_aggr)
return(join_range)
}
