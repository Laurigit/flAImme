odds_of_blocked_slots_phase2 <- function(game_status, move_range, STG_CYCLER, cycler_id, phase_two_cyclers) {
  #move_range <- range_data
  #track width

  width <- game_status[, .(Track_width = .N), by = GAME_SLOT_ID]

  move_order <- game_status[CYCLER_ID > 0, .(CYCLER_ID, SQUARE_ID)][order(-SQUARE_ID)]
  move_order[, order := seq_len(.N)]
  relevant_cyclers <- move_order[order < move_order[CYCLER_ID == cycler_id, order], CYCLER_ID]
  #only care about those who move before me

  #cycler_id is the one considering moving and his teammate is not moving

  #move_range <- calc_slot
  cycler_posits <- game_status[CYCLER_ID > 0, .(posit = max(GAME_SLOT_ID)), by = CYCLER_ID]
  join_team <- STG_CYCLER[cycler_posits, on = "CYCLER_ID"]



  join_range_all <- join_team[move_range, on = "CYCLER_ID"]
  join_range <- join_range_all[ CYCLER_ID %in% relevant_cyclers]
  if (nrow(join_range) > 1) {

  # phase_two_cyclers <- c(1,3,5,7)
   join_range[, phase_two_cycler := CYCLER_ID %in% phase_two_cyclers]



    #split odds by half. Chosee randoml who to move first
    new_pos_data <- join_range[phase_two_cycler == TRUE, .( TEAM_ID, CYCLER_ID, slot = new_slot, shared_odds)]
    old_pos_data <- join_range[phase_two_cycler == FALSE, .( TEAM_ID, CYCLER_ID, slot = posit, shared_odds = 0.9999)]
    appendaa <- rbind(new_pos_data, old_pos_data)
    aggr_to_events_and_slots <- appendaa[, .(count_cyclers =.N), by = .(TEAM_ID, slot, shared_odds)]

    #first count odds where count_cyclers = 1. Meaning only one per team can end up to that slot
    joined_width <- width[aggr_to_events_and_slots, on = .(GAME_SLOT_ID = slot)]

    oneCycler <- joined_width[, .(cp.quadratic(unlist(as.list(shared_odds)), Track_width)), by = GAME_SLOT_ID]

    result_data <- oneCycler[, .(resultti = V1, GAME_SLOT_ID)][, CYCLER_ID := cycler_id]
    convert_res_back_to_MOVEMENT <- move_range[result_data, on = .(new_slot = GAME_SLOT_ID, CYCLER_ID)][!is.na(MOVEMENT)]

    ss_res <- convert_res_back_to_MOVEMENT[resultti > 0,. (CYCLER_ID, MOVEMENT, blocked_odds = resultti )]
    return(ss_res)
  } else {
    return(NULL)
  }
}

