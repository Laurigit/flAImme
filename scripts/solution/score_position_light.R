score_position_light <- function(game_status, ADM_AI_CONF,  precalc_track, ctM_data, orig_posits, STG_CYCLER, action_data) {
  # action_databr <- data.table(CYCLER_ID = c(1,2,3,4,5,6), MOVEMENT = c(2,3,2,5,4,3), move_order = c(1,2,3,4,5,6), TEAM_ID = c(1,1,2,2,3,3))

  #make moves
  temp_game_status <- game_status
  #zoom(temp_game_status)
  new_cycler_positions <- temp_game_status[CYCLER_ID > 0, .(CYCLER_ID, GAME_SLOT_ID)]

  #cycler order
  CYCLER_ORDER <- temp_game_status[CYCLER_ID > 0][order(-SQUARE_ID)][, .(CYCLER_ID, Score = -seq_len(.N), Setting = "Move_order",
                                                                         modifier = NA, Setting_modifier = "")]



  #new speed
  #who has the lowest turns to finish? then compare each cycler against that
  #expect the one who is leading compares against the second



  turns_to_finish <- ctM_data[new_cycler_positions, on = .(CYCLER_ID, new_slot_after_moving = GAME_SLOT_ID)][, .(.N), by = .(CYCLER_ID, turns_to_finish)]
  leader_turns <- turns_to_finish[, min(turns_to_finish)]
  second_turns <- turns_to_finish[turns_to_finish > leader_turns, max(turns_to_finish)]
  if (is.infinite(second_turns)) {
    second_turns <- leader_turns
  }
  turns_to_finish[, is_leader := turns_to_finish == leader_turns]
  turns_to_finish[, ttf_score := ifelse(is_leader,
                                        abs(second_turns - turns_to_finish),
                                        leader_turns - turns_to_finish + 1), by = CYCLER_ID]
  SPEED_CHANGE <- turns_to_finish[, .(Score = ttf_score , CYCLER_ID, Setting = "Speed_change",
                                      modifier = NA, Setting_modifier = "")]


  #exhaust

  exhaust_amount_temp <- calc_exhaust(temp_game_status)
  #less exhaust care after halfway
  ss_turns_to_finish <- turns_to_finish[, .(turns_to_finish, CYCLER_ID)]
  #join turns
  join_turns_exh <- ss_turns_to_finish[exhaust_amount_temp, on = "CYCLER_ID"]

  EXHAUST_AMOUONT <- join_turns_exh[,.(CYCLER_ID, Score = -EXHAUST , Setting = "Get_exhaust", modifier = turns_to_finish, Setting_modifier = 1)]


  #MOVEMENT GAINED PHASE 2

  new_posits <- temp_game_status[CYCLER_ID > 0, .(GAME_SLOT_ID_NEW = GAME_SLOT_ID, turns_out_of_mountain_new = turns_out_of_mountain), by = CYCLER_ID]

  MOVEMENT_GAINED <- orig_posits[new_posits, on = "CYCLER_ID"][, .(Score = GAME_SLOT_ID_NEW - GAME_SLOT_ID, CYCLER_ID, Setting = "Movement_gained",
                                                                   modifier = NA, Setting_modifier = "" )]

  join_team <- STG_CYCLER[new_cycler_positions, on = "CYCLER_ID"]
  team_penalty_temp  <- join_team[, .(Score = -abs(GAME_SLOT_ID - mean(GAME_SLOT_ID)), CYCLER_ID), by = .(TEAM_ID)]
  TEAM_PENALTY <- team_penalty_temp[, .(CYCLER_ID, Score, Setting = "Cycler_distance",
                                        modifier = NA, Setting_modifier = "")]

  #cost of card spent
  COST_OF_CARD_SPENT <- action_data[,. (CYCLER_ID, Score = -MOVEMENT, Setting = "Card_spent",
                                        modifier = NA, Setting_modifier = "")]

# SLOTS_OVER_FINISHLINE #TO MOVITAVE HIGH MOVE AT THE END
new_cycler_squares <- temp_game_status[CYCLER_ID > 0, .(CYCLER_ID, SQUARE_ID)]
action_data
join_pos_and_action <- action_data[new_cycler_squares, on = "CYCLER_ID"]
finish_square <- game_status[FINISH == 1, min(SQUARE_ID)] - 1
join_pos_and_action[, over_finish_square := max(SQUARE_ID - finish_square, 0)]
SQUARES_OVER_FINISHLINE <- join_pos_and_action[, .(CYCLER_ID, Score = over_finish_square*(10^(2 - phase)), Setting = "Finish_sprint",
                                                   modifier = NA, Setting_modifier = "")]


  position_score <- rbind(
    EXHAUST_AMOUONT,
    TEAM_PENALTY,
    #TRACK_POSITION,
    #MOUNTAIN_TURNS_SCORE,
    #EXPECTED_ASCEND_ADVANTAGE,
    #EXPECTED_ASCEND_HIT_BONUS,
    COST_OF_CARD_SPENT,
    CYCLER_ORDER,
    MOVEMENT_GAINED,
    SQUARES_OVER_FINISHLINE,
    SPEED_CHANGE)

  #join score
  POSITION_SCORE <- ADM_AI_CONF[position_score, on = .(Setting,  CYCLER_ID)]
  POSITION_SCORE[, Result := ifelse(is.na(modifier), Value * Score, Value * Score * modifier * as.numeric(Setting_modifier))]

  #join action data
  join_action <- action_data[POSITION_SCORE, on = "CYCLER_ID"]

  return(join_action)
}
