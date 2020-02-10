score_position <- function(game_status, deck_status, action_data, ADM_AI_CONF, orig_speed, precalc_track, cycler_ids) {
 # action_data <- data.table(CYCLER_ID = c(1,2,3,4,5,6), MOVEMENT = c(2,3,2,5,4,3), move_order = c(1,2,3,4,5,6), TEAM_ID = c(1,1,2,2,3,3))
  temp_deck_status <- deck_status[1 != 0, .(CYCLER_ID, CARD_ID, Zone, MOVEMENT, row_id)]
  #make moves
  temp_game_status <- game_status[1 != 0, .(PIECE_ATTRIBUTE,
                                            FINISH,
                                            GAME_SLOT_ID,
                                            LANE_NO,
                                            SQUARE_ID,
                                            CYCLER_ID,
                                            EXTRA,
                                            turns_out_of_mountain)]



  #MOVEMENT gained phase one. k posisck orig positions
  orig_posits <- temp_game_status[CYCLER_ID > 0, .(GAME_SLOT_ID, turns_out_of_mountain), by = CYCLER_ID]



  #best_speed <- orig_speed[, .(Speed = min(TURN_ID * 10 - row_over_finish)), by = CYCLER_ID]
    #blocks not needed most likely
  # BLOCK_ROWS <- NULL
  # BLOCKER_HONOR <- NULL

   for (move_loop in action_data[!is.na(MOVEMENT), move_order]) {
     move_data <- action_data[move_order == move_loop]
     move_amount <- move_data[, MOVEMENT]
     move_cycler_id <- move_data[, CYCLER_ID]
     temp_deck_status <- play_card(move_cycler_id, card_id = move_amount, temp_deck_status, 1, 1)
  #   block_info <-  calc_block_squares(temp_game_status, move_cycler_id, move_amount)
  #   block_data <- data.table(CYCLER_ID = move_cycler_id, Score = block_info$block_amount, Setting = "Get_blocked")
  #   block_honor_row <- cbind(block_info$block_cycler)
  #   BLOCKER_HONOR <- rbind(BLOCKER_HONOR, block_honor_row)
  #   BLOCK_ROWS <- rbind(BLOCK_ROWS, block_data)
     temp_game_status <- move_cycler(temp_game_status, move_cycler_id, move_amount, slipstream = FALSE, ignore_block = FALSE, ignore_end_of_track = TRUE)
   }

  #cycler order
  CYCLER_ORDER <- temp_game_status[CYCLER_ID > 0][order(-SQUARE_ID)][, .(CYCLER_ID, Score = seq_len(.N), Setting = "Move_order")]

  #SLIPSTREAM
  #most likely not needed
  # SLIPSTREAM <- calc_slipstream(temp_game_status)
  # slip_get_temp <- SLIPSTREAM[, .(Score = mean(SLIP_ROWS)), by = .(CYCLER_ID, slip_instance)]
  # SLIPSTREAM_GET <- slip_get_temp[,.(Score = sum(Score)), by = .(CYCLER_ID)]
  # SLIPSTREAM_GET[, Setting := "Getslipstream"]
  # slip_give_temp <- SLIPSTREAM[, .(Score = mean(SLIP_ROWS)), by = .(SR_GIVER_CYCLER_ID, slip_instance)]
  # SLIPSTREAM_GIVE <- slip_give_temp[, .(Score = sum(Score)), by = .(CYCLER_ID = SR_GIVER_CYCLER_ID)]
  # SLIPSTREAM_GIVE[, Setting := "Give_slipstream"]

   #MOVE DRIVERS
  temp_game_status <- apply_slipstream(temp_game_status)


#new speed

 # new_speed <- turns_to_finish(temp_game_status, deck_status)
  new_speed <- lapply(cycler_ids, function(x) {
    optimal_moves_to_finish(x, temp_game_status, temp_deck_status, precalc_track, use_draw_odds = TRUE)})
  speed_result <- rbindlist(new_speed)[, .(TURN_ID = Turns_to_finish), by  = .(CYCLER_ID = cycler_id)]
 # best_speed_new <- new_speed[, .(Speed_new =  min(TURN_ID * 10 - row_over_finish + 10)), by = CYCLER_ID]

  SPEED_CHANGE <- speed_result[, .(Score = TURN_ID , CYCLER_ID, Setting = "Speed_change")]




  #TRACK_POSITION

  leader <- temp_game_status[CYCLER_ID > 0, max(GAME_SLOT_ID)]
  #compare_slot
  track_posis <- temp_game_status[CYCLER_ID > 0, .(GAME_SLOT_ID, KEY = "1"), by = CYCLER_ID]
  cross_join <- merge(x=track_posis, y=track_posis, by = "KEY", allow.cartesian = TRUE)
  aggr <- cross_join[CYCLER_ID.x != CYCLER_ID.y,  .(COMPARE_GAME_SLOT_ID = quantile(GAME_SLOT_ID.y, 0.75)), by = .(CYCLER_ID = CYCLER_ID.x,
                                                                                                                   GAME_SLOT_ID = GAME_SLOT_ID.x)]

  TRACK_POSITION <- aggr[, .(Score =  GAME_SLOT_ID - COMPARE_GAME_SLOT_ID), by = CYCLER_ID]
  TRACK_POSITION[, Setting := "Track_position"]

  #TRACK_LEFT
  finish <- temp_game_status[FINISH == 1, max(GAME_SLOT_ID)]
  distance_left <-   1 - leader/finish
  #care track position after halfway
  TRACK_POSITION[, Score := Score * (0.6 - min(0.5, distance_left))]

  #exhaust

  exhaust_amount_temp <- calc_exhaust(temp_game_status)
  #less exhaust care after halfway

  EXHAUST_AMOUONT <- exhaust_amount_temp[,.(CYCLER_ID, Score = EXHAUST * distance_left, Setting = "Get_exhaust")]
  #DEAL EXHAUST ACTUALLY
  deck_status <- apply_exhaustion(deck_status, temp_game_status)



#MOVEMENT GAINED PHASE 2

  new_posits <- temp_game_status[CYCLER_ID > 0, .(GAME_SLOT_ID_NEW = GAME_SLOT_ID, turns_out_of_mountain_new = turns_out_of_mountain), by = CYCLER_ID]

  MOVEMENT_GAINED <- orig_posits[new_posits, on = "CYCLER_ID"][, .(Score = GAME_SLOT_ID_NEW - GAME_SLOT_ID, CYCLER_ID, Setting = "Movement_gained")]

  #distance between team members
  join_team <- TRACK_POSITION[action_data, on = "CYCLER_ID"]
  team_penalty_temp  <- join_team[, .(Score = abs(Score - mean(Score)), CYCLER_ID), by = .(TEAM_ID)]
  TEAM_PENALTY <- team_penalty_temp[, .(CYCLER_ID, Score, Setting = "Cycler_distance")]

#potential draws next turn
  DH_POTENTIAL <- NULL
  POTENTIAL_DRAWS <- NULL
  for(cycler_loop in cyclers) {
    row_res <- data.table(CYCLER_ID = cycler_loop, MOVEMENT = getDownHillMoves(temp_game_status, cycler_loop))
    potential_draws <- calculate_potential_next_draw(cycler_loop, deck_status[CYCLER_ID == cycler_loop])
    POTENTIAL_DRAWS <-  rbind(POTENTIAL_DRAWS, potential_draws)
    join_pot <- row_res[potential_draws, on = .(MOVEMENT)][!is.na(CYCLER_ID)][, i.CYCLER_ID := NULL]
    DH_POTENTIAL <- rbind(DH_POTENTIAL, join_pot)
  }

  #zoom(temp_game_status)




how_many_cards_to_draw <- 4
# POTENTIAL_DRAWS <- POTENTIAL_DRAWS[CYCLER_ID < 3][order(MOVEMENT)]
# POTENTIAL_DRAWS[6:7, draw_round := 0]
# POTENTIAL_DRAWS[, CYCLER_ID := 1]

#calculate_odds_of_drawing
aggr_by_movement <- POTENTIAL_DRAWS[, .(movement_count = .N), by = .(MOVEMENT, draw_round, CYCLER_ID)]

aggr_by_movement[, potential_draws_card_count := sum(movement_count), by = .(CYCLER_ID, draw_round)]
aggr_by_movement[, other_cards := potential_draws_card_count - movement_count]
#calculate how many cards we drew on round 0
aggr_by_movement[, cards_drawn := sum((1 - draw_round) * movement_count), by = CYCLER_ID]
aggr_by_movement[, draw_x_cards := how_many_cards_to_draw - cards_drawn]
aggr_by_movement[, DRAW_ODDS_MIN_ONE := ifelse(draw_round == 0, 1, 1- phyper(0, movement_count, other_cards, how_many_cards_to_draw - cards_drawn))]

#then calculate that what is the odds of it being smallest or largest card

#self_join
#aggr_by_movement<- aggr_by_movement[CYCLER_ID == 1]
self_join <- merge(aggr_by_movement, aggr_by_movement, by = c("CYCLER_ID", "draw_x_cards", "potential_draws_card_count"), allow.cartesian = TRUE)
self_join[, ':=' (HIGHEST_CERTAIN_MOVEMENT = max(ifelse(draw_round.x == 0, MOVEMENT.x, NA), na.rm = TRUE),
                  SMALLEST_CERTAIN_MOVEMENT = min(ifelse(draw_round.x == 0, MOVEMENT.x, NA), na.rm = TRUE))]
filter_data <- self_join[(MOVEMENT.x > MOVEMENT.y) & draw_round.x > 0 & draw_round.y > 0]
aggre_count <- filter_data[, .(count_of_cards_lower_row =  sum(movement_count.y)), by = .(draw_x_cards, movement_count.x, HIGHEST_CERTAIN_MOVEMENT, SMALLEST_CERTAIN_MOVEMENT, DRAW_ODDS_MIN_ONE.x, CYCLER_ID, MOVEMENT.x, potential_draws_card_count)]
aggre_count[, white_balls := potential_draws_card_count - count_of_cards_lower_row]
aggre_count[, black_balls := count_of_cards_lower_row]
aggre_count[, odds_of_all_cards_smaller_than_X := ifelse(MOVEMENT.x > HIGHEST_CERTAIN_MOVEMENT,
                                                                  phyper( 0,
                                                                              white_balls,
                                                                          black_balls,

                                                                          draw_x_cards),
                                                                    0)]
aggre_count[, odds_of_drawing_X_and_none_of_the_rest_are_higher :=   shift(odds_of_all_cards_smaller_than_X, type = c("lead"), n = 1) - odds_of_all_cards_smaller_than_X, by = CYCLER_ID]
aggre_count[is.na(odds_of_drawing_X_and_none_of_the_rest_are_higher), odds_of_drawing_X_and_none_of_the_rest_are_higher :=  1 - odds_of_all_cards_smaller_than_X ]
aggre_count[, odds_of_drawing_X_or_lower := ifelse(MOVEMENT.x >= SMALLEST_CERTAIN_MOVEMENT, 1,
                                                   phyper(0,
                                                          potential_draws_card_count - count_of_cards_lower_row - movement_count.x,
                                                          count_of_cards_lower_row + movement_count.x,
                                                          draw_x_cards))]

filter_data_calc_lowest <- self_join[(MOVEMENT.x < MOVEMENT.y)  & draw_round.x > 0 & draw_round.y > 0]
aggre_count_lowest <- filter_data_calc_lowest[, .(count_of_cards_higher_row =  sum(movement_count.y)), by = .(draw_x_cards, movement_count.x, HIGHEST_CERTAIN_MOVEMENT, SMALLEST_CERTAIN_MOVEMENT,DRAW_ODDS_MIN_ONE.x, CYCLER_ID, MOVEMENT.x, potential_draws_card_count)]
aggre_count_lowest[, white_balls := potential_draws_card_count - count_of_cards_higher_row]
aggre_count_lowest[, black_balls := count_of_cards_higher_row]
aggre_count_lowest[, odds_of_all_cards_higher_than_X :=  ifelse(MOVEMENT.x < SMALLEST_CERTAIN_MOVEMENT, phyper(0,
                                                                   white_balls,
                                                                   black_balls,
                                                                   draw_x_cards),
                                                                0)]
aggre_count_lowest[, odds_of_drawing_X_and_none_of_the_rest_are_smaller :=     shift(odds_of_all_cards_higher_than_X, type = c("lag"), n = 1) - odds_of_all_cards_higher_than_X, by = CYCLER_ID]
aggre_count_lowest[is.na(odds_of_drawing_X_and_none_of_the_rest_are_smaller), odds_of_drawing_X_and_none_of_the_rest_are_smaller := 1 - odds_of_all_cards_higher_than_X ]
aggre_count_lowest[, odds_of_drawing_X_or_higher := ifelse(MOVEMENT.x <= HIGHEST_CERTAIN_MOVEMENT, 1,
                                                           1 -phyper(0,
                                                                  count_of_cards_higher_row + movement_count.x,
                                                                  potential_draws_card_count - count_of_cards_higher_row - movement_count.x,

                                                                  draw_x_cards))]
#joinaa
# ss_cols_highest <- aggre_count[, .(CYCLER_ID, MOVEMENT = MOVEMENT.x, DRAW_ODDS_MIN_ONE = DRAW_ODDS_MIN_ONE.x, HAND_HIGHEST_X_OR_LOWER = odds_of_drawing_at_least_this_or_smaller,
#                                    HAND_CONTAINS_X_BUT_NOT_HIGHER = odds_of_being_highest_drawn_card)]
ss_cols_highest <- aggre_count[, .(CYCLER_ID, MOVEMENT = MOVEMENT.x, DRAW_ODDS_MIN_ONE = DRAW_ODDS_MIN_ONE.x,
                                   HAND_CONTAINS_X_OR_SMALLER = odds_of_drawing_X_or_lower,
                                   HAND_CONTAINS_X_BUT_NOT_HIGHER = odds_of_drawing_X_and_none_of_the_rest_are_higher)]
# ss_cols_lowest <- aggre_count_lowest[, .(CYCLER_ID, MOVEMENT = MOVEMENT.x, DRAW_ODDS_MIN_ONE = DRAW_ODDS_MIN_ONE.x, HAND_CONTAINS_CARD_X_OR_LESS = odds_of_drawing_this_or_greater,
#                                    HAND_CONTAINS_X_BUT_NOT_LOWER = odds_of_this_being_the_highest_movement_in_hand)]
ss_cols_lowest <- aggre_count_lowest[, .(CYCLER_ID, MOVEMENT = MOVEMENT.x, DRAW_ODDS_MIN_ONE = DRAW_ODDS_MIN_ONE.x,
                                         HAND_CONTAINS_X_OR_HIGHER = odds_of_drawing_X_or_higher,
                                         HAND_CONTAINS_X_BUT_NOT_LOWER = odds_of_drawing_X_and_none_of_the_rest_are_smaller)]

join_odds <- merge(x = ss_cols_highest, y = ss_cols_lowest, by = c("MOVEMENT", "CYCLER_ID", "DRAW_ODDS_MIN_ONE"), all = TRUE)
join_odds[, ':=' (HAND_CONTAINS_X_OR_HIGHER = ifelse(is.na(HAND_CONTAINS_X_OR_HIGHER), DRAW_ODDS_MIN_ONE, HAND_CONTAINS_X_OR_HIGHER),
                  HAND_CONTAINS_X_BUT_NOT_HIGHER = ifelse(is.na(HAND_CONTAINS_X_BUT_NOT_HIGHER), 0, HAND_CONTAINS_X_BUT_NOT_HIGHER),
                  HAND_CONTAINS_X_OR_SMALLER = ifelse(is.na(HAND_CONTAINS_X_OR_SMALLER), DRAW_ODDS_MIN_ONE, HAND_CONTAINS_X_OR_SMALLER),
                  HAND_CONTAINS_X_BUT_NOT_LOWER =  ifelse(is.na(HAND_CONTAINS_X_BUT_NOT_LOWER), 0, HAND_CONTAINS_X_BUT_NOT_LOWER),
                  draw_round = 1)]

#aggreagate with draw_round = 0
aggr_draw_0 <- join_odds[POTENTIAL_DRAWS, on = .(MOVEMENT, CYCLER_ID, draw_round)]
aggr_draw_0[is.na(aggr_draw_0)] <- 1
POTENTIAL_DRAWS_WITH_ODDS <- aggr_draw_0[, .(DRAW_ODDS_MIN_ONE = max(DRAW_ODDS_MIN_ONE),
                            HAND_CONTAINS_X_OR_SMALLER = max(HAND_CONTAINS_X_OR_SMALLER),
                            HAND_CONTAINS_X_BUT_NOT_HIGHER = max(HAND_CONTAINS_X_BUT_NOT_HIGHER),
                            HAND_CONTAINS_X_OR_HIGHER = max(HAND_CONTAINS_X_OR_HIGHER),
                            HAND_CONTAINS_X_BUT_NOT_LOWER = max(HAND_CONTAINS_X_BUT_NOT_LOWER)), by = .(CYCLER_ID, MOVEMENT)]



#landing to ascend
ascend_row_bool <- temp_game_status[CYCLER_ID > 0, .(CYCLER_ID, ASCEND = ifelse(PIECE_ATTRIBUTE == "A", 1, 0))]
#join potential to use ascend
POTENTIAL_DRAWS_WITH_ODDS[, ascend_advantage := pmax(5 - MOVEMENT, 0)]
join_ascpot <- ascend_row_bool[POTENTIAL_DRAWS_WITH_ODDS, on = .(CYCLER_ID)]

EXPECTED_ASCEND_ADVANTAGE <- join_ascpot[, .(Score = pmin(((sum(HAND_CONTAINS_X_OR_SMALLER * ascend_advantage * ASCEND ))), 3)), by = CYCLER_ID]
EXPECTED_ASCEND_ADVANTAGE[, Setting := "Dh_square"]
#EXPECTED probability to hit ASCEND next turn

dh_ss <- DH_POTENTIAL[, .(.N), by = .(CYCLER_ID, MOVEMENT)]
temp_expected_dh_odds <- dh_ss[POTENTIAL_DRAWS_WITH_ODDS, on = .(CYCLER_ID, MOVEMENT)][!is.na(N),. (CYCLER_ID, DRAW_ODDS_MIN_ONE, MOVEMENT)]
if (nrow(temp_expected_dh_odds) == 0) {
  EXPECTED_ASCEND_HIT_BONUS <-  data.table(CYCLER_ID = numeric(), Score = numeric(), Setting = character())
} else {
EXPECTED_ASCEND_HIT_BONUS <- temp_expected_dh_odds[, .(Score = max(DRAW_ODDS_MIN_ONE)), by = .( CYCLER_ID)][, Setting := "Dh_option"]
}

#increased mountain turns length

mountain_score <- orig_posits[new_posits, on = "CYCLER_ID"][, .(Score =  turns_out_of_mountain - turns_out_of_mountain_new, CYCLER_ID, Setting = "Extra_mountain_turn")]
mountain_score_odds <- POTENTIAL_DRAWS_WITH_ODDS[mountain_score, on = "CYCLER_ID"][MOVEMENT %in% c(5, 6, 7)]
MOUNTAIN_TURNS_SCORE <- mountain_score_odds[, .(Score =  max(Score *HAND_CONTAINS_X_OR_HIGHER)), by = .(CYCLER_ID, Setting)]
#cost of card spent
COST_OF_CARD_SPENT <- action_data[,. (CYCLER_ID, Score = MOVEMENT, Setting = "Card_spent")]

position_score <- rbind(
#BLOCKER_HONOR,
#BLOCK_ROWS,
#SLIPSTREAM_GET,
#SLIPSTREAM_GIVE,
EXHAUST_AMOUONT,
TEAM_PENALTY,
TRACK_POSITION,
MOUNTAIN_TURNS_SCORE,
EXPECTED_ASCEND_ADVANTAGE,
EXPECTED_ASCEND_HIT_BONUS,
COST_OF_CARD_SPENT,
CYCLER_ORDER,
MOVEMENT_GAINED,
SPEED_CHANGE)

#join score
POSITION_SCORE <- ADM_AI_CONF[position_score, on = .(Setting,  CYCLER_ID)]
POSITION_SCORE[, Result := Value * Score]
return(POSITION_SCORE)
}
