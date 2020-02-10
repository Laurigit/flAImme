odds_of_blocked_slots <- function(game_status, move_range, STG_CYCLER, cycler_id) {
#Jäit kohtaan missä yritit miettiä, miten käsitellään tapaukset, missä tiimin pyöräilijät lähtee samasta ruudusta tai uhkaa päätyä samaan
  #move_range <- range_data
  #track width
  width <- game_status[, .(Track_width = .N), by = GAME_SLOT_ID]

  move_order<- game_status[CYCLER_ID > 0, .(CYCLER_ID, SQUARE_ID)][order(-SQUARE_ID)]
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
  #join_range_all[CYCLER_ID ==2, tm_position := 6]
  #join_width
  # join_width <- width[join_range_all, on = .(GAME_SLOT_ID = new_slot)]


  join_range <- join_range_all[TEAM_ID != my_team & CYCLER_ID %in% relevant_cyclers]
  if (nrow(join_range) > 1) {



  #split odds by half. Chosee randoml who to move first
  join_range[, split_odds := shared_odds / 2]
  join_range[, event_id := seq_len(.N)]
  new_pos_data <- join_range[, .(event_id, TEAM_ID, CYCLER_ID, slot = new_slot, split_odds)]
  old_pos_data <- join_range[, .(event_id, TEAM_ID, CYCLER_ID = teamMate_CYCLER_ID, slot = tm_position, split_odds)]
  appendaa <- rbind(new_pos_data, old_pos_data)
  aggr_to_events_and_slots <- appendaa[, .(count_cyclers =.N), by = .(event_id, TEAM_ID, slot, split_odds)]

  #first count odds where count_cyclers = 1. Meaning only one per team can end up to that slot
  joined_width <- width[aggr_to_events_and_slots, on = .(GAME_SLOT_ID = slot)]
  aggr_odds_per_team <- joined_width[, .(sm_odds = sum(split_odds)), by = .(GAME_SLOT_ID, Track_width, TEAM_ID, count_cyclers)]
  aggr_odds_per_team[ sm_odds > 0.99, sm_odds := 0.9999]
  oneCycler <- aggr_odds_per_team[count_cyclers == 1, .(cp.quadratic(unlist(as.list(sm_odds)), Track_width)), by = GAME_SLOT_ID]

  #then combine odds of the case when one team occupies two slots
  exception_case  <- aggr_odds_per_team[count_cyclers == 2]
  #join normal case
  norm_joined <- exception_case[oneCycler, on = .(GAME_SLOT_ID)]
  norm_joined[, slot_blocked :=  (1 - V1) * sm_odds + V1 * (1 - sm_odds) + V1 * sm_odds]
  norm_joined[, resultti := ifelse(is.na(slot_blocked), V1, slot_blocked)]
  result_data <- norm_joined[, .(resultti, GAME_SLOT_ID)][, CYCLER_ID := cycler_id]
  convert_res_back_to_MOVEMENT <- move_range[result_data, on = .(new_slot = GAME_SLOT_ID, CYCLER_ID)][!is.na(MOVEMENT)]
  ss_res <- convert_res_back_to_MOVEMENT[resultti > 0,. (CYCLER_ID, MOVEMENT, blocked_odds = resultti )]
  return(ss_res)
  } else {
    return(NULL)
  }
}

