score_position <- function(game_status, deck_status, action_data) {
  #action_data <- data.table(CYCLER_ID = c(1,2,3,4,5,6), MOVEMENT = c(2,3,2,5,4,3), move_order = c(1,2,3,4,5,6))

  #make moves
  temp_game_status <- game_status[1 != 0, .(PIECE_ATTRIBUTE,
                                            FINISH,
                                            GAME_SLOT_ID,
                                            LANE_NO,
                                            SQUARE_ID,
                                            CYCLER_ID)]

  BLOCK_ROWS <- NULL
  BLOCKER_HONOR <- NULL
  for (move_loop in action_data[, move_order]) {
    move_data <- action_data[move_order == move_loop]
    move_amount <- move_data[, MOVEMENT]
    move_cycler_id <- move_data[, CYCLER_ID]
    block_info <-  calc_block_squares(temp_game_status, move_cycler_id, move_amount)
    block_data <- data.table(CYCLER_ID = move_cycler_id, block_amount = block_info$block_amount)
    block_honor_row <- block_info$block_cycler
    BLOCKER_HONOR <- rbind(BLOCKER_HONOR, block_honor_row)
    BLOCK_ROWS <- rbind(BLOCK_ROWS, block_data)
    temp_game_status <- move_cycler(temp_game_status, move_cycler_id, move_amount, slipstream = FALSE, ignore_block = FALSE)
  }

  #cycler order
  CYCLER_ORDER <- game_status[CYCLER_ID > 0][order(-SQUARE_ID)][, .(CYCLER_ID, move_order = seq_len(.N))]

  #SLIPSTREAM

  SLIPSTREAM <- calc_slipstream(temp_game_status)
  temp_game_status <- apply_slipstream(temp_game_status)

  #exhaust
  EXHAUST_AMOUNT <- calc_exhaust(temp_game_status)
  deck_status <- apply_exhaustion(deck_status, temp_game_status)


  #TRACK_POSITION
  leader <- temp_game_status[CYCLER_ID > 0, max(GAME_SLOT_ID)]
  TRACK_POSITION <- temp_game_status[CYCLER_ID > 0, .(relative_track_position = leader - GAME_SLOT_ID), by = CYCLER_ID]



  #increased mountain turns length
  cyclers <- game_state[CYCLER_ID > 0, CYCLER_ID]
  MOUNTAIN_TURNS <- data.table(CYCLER_ID = cyclers, orig_length = 0, new_length = 0)
  for(cycler_loop in cyclers) {
    result_orig <- slots_out_of_mountains(game_state, cycler_loop)
    result_new <- slots_out_of_mountains(temp_game_status, cycler_loop)
    MOUNTAIN_TURNS[CYCLER_ID == cycler_loop,':=' (orig_length = result_orig, new_length = result_new)]
  }
  MOUNTAIN_TURNS[, mountain_turn_change := ifelse(new_length > 0, ceiling(new_length / 5) - ceiling(orig_length / 5) + 1, 0)]

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
#POTENTIAL_DRAWS[4:8, draw_round := 0]
#calculate_odds_of_drawing
aggr_by_movement <- POTENTIAL_DRAWS[, .(movement_count = .N), by = .(MOVEMENT, draw_round, CYCLER_ID)]

aggr_by_movement[, potential_draws_card_count := sum(movement_count), by = .(CYCLER_ID, draw_round)]
aggr_by_movement[, other_cards := potential_draws_card_count - movement_count]
#calculate how many cards we drew on round 0
aggr_by_movement[, cards_drawn := sum((1 - draw_round) * movement_count), by = CYCLER_ID]
aggr_by_movement[, DRAW_ODDS_MIN_ONE := ifelse(draw_round == 0, 1, 1- phyper(0, movement_count, other_cards, how_many_cards_to_draw - cards_drawn))]

#then calculate that what is the odds of it being smallest or largest card

#self_join
#aggr_by_movement<- aggr_by_movement[CYCLER_ID == 1]
self_join <- merge(aggr_by_movement, aggr_by_movement, by = "CYCLER_ID", allow.cartesian = TRUE)
filter_data <- self_join[(MOVEMENT.x > MOVEMENT.y)]
aggre_count <- filter_data[, .(count_of_cards_lower_row =  sum(movement_count.y)), by = .(DRAW_ODDS_MIN_ONE.x, CYCLER_ID, MOVEMENT.x, potential_draws_card_count.y)]
aggre_count[, odds_of_drawing_at_least_this_or_smaller := 1 - phyper(0,

                                                                     potential_draws_card_count.y - count_of_cards_lower_row,
                                                                     count_of_cards_lower_row,
                                                                     4)]
aggre_count[, odds_of_this_being_the_highest_movement :=  odds_of_drawing_at_least_this_or_smaller - shift(odds_of_drawing_at_least_this_or_smaller, type = c("lead"), n = 1)]
filter_data_prod <- self_join[(MOVEMENT.x < MOVEMENT.y)]
aggre_tulo <- filter_data_prod[, .(prod_of_odds =  prod(DRAW_ODDS_MIN_ONE.y)), by = .(CYCLER_ID, MOVEMENT.x)]
aggre[, odds_of_this_being_highest := ]
self_join_filter <- self_join[, .(odds_of_drawing_higher_card = sum(DRAW_ODDS.y * (MOVEMENT.x < MOVEMENT.y))), by = .(MOVEMENT.x, DRAW_ODDS.x)]
self_join_filter[, ODDS_OF_HIGHEST_CARD := DRAW_ODDS.x * (1 - odds_of_drawing_higher_card)]

draw_odds <- aggr_by_movement[, .(DRAW_ODDS = max(DRAW_ODDS)), by = .(MOVEMENT, CYCLER_ID)]
#calculate odds that MOVEMENT X is the highest card

POTENTIAL_DRAWS_WITH_ODDS <- POTENTIAL_DRAWS[DRAW_ODDS, on = .(MOVEMENT, CYCLER_ID)]
#POTENTIAL_DRAWS_WITH_ODDS[, draw_round := NULL]
#landing to ascend
ascend_row_bool <- temp_game_status[CYCLER_ID > 0, .(CYCLER_ID, ASCEND = ifelse(PIECE_ATTRIBUTE == "A", 1, 0))]
#join potential to use ascend
POTENTIAL_DRAWS_WITH_ODDS[, ascend_advantage := pmax(5 - MOVEMENT, 0)]
join_ascpot <- ascend_row_bool[POTENTIAL_DRAWS_WITH_ODDS, on = .(CYCLER_ID)]
#first check what advantege is certain. that 1 - 0 means that only use the 100% cards
sure_advantage <- join_ascpot[, .(max_certain_advantage = (max(ascend_advantage * (1 - draw_round)))), by = CYCLER_ID]
join_sure_adv <- join_ascpot[sure_advantage, on = "CYCLER_ID"]
EXPECTED_ASCEND_ADVANTAGE <- join_sure_adv[, .(EV = sum(DRAW_ODDS * pmax((ascend_advantage - max_certain_advantage), 0)))]#, by = CYCLER_ID]
join_sure_adv[, EV := (DRAW_ODDS * pmax((ascend_advantage - max_certain_advantage), 0))]
}
