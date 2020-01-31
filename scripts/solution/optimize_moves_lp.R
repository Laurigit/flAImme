optimize_moves <- function(game_status, deck_status, cycler_ids){

  temp_game_status_for_debugging <- game_status[1 != 0 , .(GAME_SLOT_ID,
                                                    PIECE_ATTRIBUTE,
                                                    START,
                                                    FINISH,
                                                    EXTRA,
                                                    LANE_NO,
                                                    SQUARE_ID,
                                                    CYCLER_ID)]
  cycler_id <- 1
  #temp_game_status_for_debugging <- set_cycler_position(cycler_id, 10, 1, temp_game_status_for_debugging)
  zoom(temp_game_status_for_debugging)
  cycler_ids <- temp_game_status_for_debugging[CYCLER_ID > 0, CYCLER_ID]

  cards_to_play <- deck_status[CYCLER_ID == cycler_id & Zone != "Removed", .(MOVEMENT, row_id)]
  cycler_pos <- temp_game_status_for_debugging[CYCLER_ID == cycler_id, GAME_SLOT_ID]


  aggr_to_slots <- temp_game_status_for_debugging[is.na(EXTRA)
                                                   # GAME_SLOT_ID <= game_status[FINISH == 1, GAME_SLOT_ID] #&
                                                  #  GAME_SLOT_ID >= cycler_pos
                                                  , .N, by = .(GAME_SLOT_ID, PIECE_ATTRIBUTE, FINISH)]




aggr_to_slots[, mountain_row := ifelse(PIECE_ATTRIBUTE == "M", 1, 0)]
  #aggr_to_slots[, ascend_row := ifelse(PIECE_ATTRIBUTE == "A", 1, 0)]
#aggr_to_slots[, ascend_bonus_calc := rowid(rleid(ascend_row))]
    reverse <- aggr_to_slots[order(-GAME_SLOT_ID)]
    reverse[, ascend_bonus_zone := ifelse(shift(PIECE_ATTRIBUTE, 5, type = "lead") == "A", 1, 0)]
    reverse[, ascend_bonus_zone := ifelse(is.na(ascend_bonus_zone), 0, ascend_bonus_zone)]
     reverse[, start_of_restricted_movement := ifelse(shift(mountain_row == 1, n = 6) | mountain_row == 1, 1, 0)]
  # reverse[, start_of_restricted_movement := ifelse(is.na(start_of_restricted_movement), 0, start_of_restricted_movement)]
  #reverse[, counter_cons_piece := rowid(rleid(mountain_row))]
  reverse[, counter_cons_piece_attribute := rowid(rleid(PIECE_ATTRIBUTE))]
  reverse[, track_type_group := rleid(PIECE_ATTRIBUTE)]
  #reverse[, slots_to_ascend := rowid(rleid(ascend_row))]
  reverse[, covered := 0]
  #reverse[, optimal := -1]

  #N groups greater than max card in hand
  track_groups_all <- reverse[ascend_bonus_zone != 1 &
                                PIECE_ATTRIBUTE == "N", .(
                            max_length = max(counter_cons_piece_attribute),
                          max_slot = max(GAME_SLOT_ID),
                            min_slot = min(GAME_SLOT_ID),
                            track_group_length = max(counter_cons_piece_attribute)), by = .(track_type_group)][order(track_group_length)]
  track_groups <- track_groups_all[max_length >= cards_to_play[, max(MOVEMENT)]]
  track_groups[, knapsack_id := seq_len(.N)]


  #use knapsack for cards greater than 5

  valueMapTable <- data.table(MOVEMENT = c(2,3,4,5,6,7,9), value = c(200,310,430,540,650,769,999), capped_movement = c(2,3,4,5,5,5,5))
  join_value <- valueMapTable[cards_to_play, on = .(MOVEMENT)]
  big_cards <- join_value[MOVEMENT > 5]


  first_knapsack  <- adagio::mknapsack(big_cards[, value], big_cards[, MOVEMENT], track_groups[,track_group_length])
  join_result <- cbind(data.table(knapsack_id = first_knapsack$ksack), big_cards)

  join_result_total <- track_groups[join_result, on = "knapsack_id"]
  aggr_res <- join_result_total[, .(cards_played = list(row_id),
                                    tot_movement = sum(MOVEMENT), max_slot = max(max_slot),
                                    min_slot = min(min_slot)), .(track_type_group, track_group_length)][order(min_slot)]

  #do we start from big card or small card?


  #my curr position
  curr_position <- temp_game_status_for_debugging[CYCLER_ID == cycler_id, GAME_SLOT_ID]
  #if min_slot_id > current position, we need to reach there first before playing big cards.
  if (aggr_res[, min(min_slot, na.rm = TRUE)] > curr_position) {
      #need to fill gap between curr position and next big card

    #inputs, game_status, deck_status, which slot to reach at minimum. Which slot to reach at maximum
    #output, cards played, new track position



  } else {
    #it is now optimal to start with a big card. We can play them

    #this can stay
    movement_db <- aggr_res[which.min(min_slot), tot_movement]
    curr_slot <- game_status
    #deck_left <- deck_status[CYCLER_ID == cycler_id & !row_id %in% join_result_total[, row_id]]

    #this is the first optimal decision
    #DEBUGGINNG
    movement_db <- aggr_res[which.min(min_slot), tot_movement]
    temp_game_status_for_debugging <- move_cycler(temp_game_status_for_debugging, cycler_id, movement_db)
    zoom(temp_game_status_for_debugging, 10, 10)
    deck_status_played <- deck_status[1 != 3]
    cards_played_row_id <- join_result_total[join_result_total[, min(min_slot, na.rm = TRUE)] == min_slot, row_id]
    for (cardLoop in cards_played_row_id) {
      deck_status_played <- play_card(cycler_id, card_id = NULL, game_id = 1, turn_id = 1, con = FALSE,
                                      current_decks =  deck_status_played, card_row_id = cardLoop)
    }

    #also, reserve cards even though they are not played
    card_reserved <-  join_result_total[join_result_total[, min(min_slot, na.rm = TRUE)] != min_slot, row_id]
    reserved_cards <- deck_status_played[1 * 1 != 4 / 2]
    for (cardLoop in card_reserved) {
      reserved_cards <- play_card(cycler_id, card_id = NULL, game_id = 1, turn_id = 1, con = FALSE,
                                  current_decks =  reserved_cards, card_row_id = cardLoop)
    }
    #DEBUGGING END
  }




  #aggr_res contains the gaps we need to fill. This is iterative as the exact start position depends on the first solution

  loop_no <- 1
  #start_pos = min_slot + tot_movement - 1,
  next_to_lp_solve <- aggr_res[, .(
                                   capacity_max = shift(max_slot, type = "lead") -  shift(tot_movement, type = "lead"),
                                   capacity_min = shift(min_slot, type = "lead") - tot_movement)]

  cap <- next_to_lp_solve[1, capacity_max]
  cap_min <- next_to_lp_solve[1, capacity_min]


  lpMappingTable <- data.table(MOVEMENT = c(2, 3, 4, 5, 6, 7, 9),
                               cost = c(1.1, 1.09, 1.08, 1.07, 1.06, 1.05, 1.04),
                               weight = c(2, 3, 4, 5, 6, 7, 9),
                               positive = c(9, 7, 6, 5, 4, 3, 2))



  #play_card(cycler_id, deck_status, game_id = 1, turn_id = 1, con = FALSE, card_id = NULL, card_row_id = )
  joinlpMpaping <- lpMappingTable[deck_left[MOVEMENT < 6], on = "MOVEMENT"]











  }



fit_cards_to_track <- function(reverse, deck_status, row_id, start_pos = NULL) {

  }

  #check if acends needed
  cards_over_5_sum <- deck_status[CYCLER_ID == cycler_id & MOVEMENT >= 5, sum(MOVEMENT)]
  track_left <- reverse[, .N]
  missing_movement <- track_left - cards_over_5_sum
  ascend_benefit <- deck_status[CYCLER_ID == cycler_id & MOVEMENT <5]
  ascend_benefit[, benefit := 5 - MOVEMENT]
  sort_benefit <- ascend_benefit[order(-benefit)]
  sort_benefit[, cum_benefit := cumsum(benefit)]

  A_road <- reverse[PIECE_ATTRIBUTE == "A", .(max_ascends = ceiling(max(counter_cons_piece_attribute) / 5)), by = track_type_group]




  reverse <- fit_card(reverse, cards_to_play)

  fit_card <- function(reverse, cards_to_play) {

    #find biggest value where attribute == N. If multiple, pick one that does not contain A
    sort_cards <- sort(cards_to_play, decreasing = TRUE)

    for(move_loop in 1:length(sort_cards)) {
      MOVEMENT <- sort_cards[move_loop]
      track_area  <- reverse[PIECE_ATTRIBUTE != "M" & MOVEMENT == (counter_cons_piece - 1) & covered == 0,
                             .(GAME_SLOT_ID = max(GAME_SLOT_ID)), by =  .(ascend_on_the_way = counter_cons_piece >= slots_to_ascend)]
      if (nrow(track_area) != 0) {
        track_area[, optimal := ifelse(ascend_on_the_way == 0, 1, 0)]
        slot_selected <- track_area[which.max(optimal)]

        game_status <- set_cycler_position(1, slot_selected[, GAME_SLOT_ID], 1, game_status)
        zoom(game_status,15)
        reverse[GAME_SLOT_ID > slot_selected[, GAME_SLOT_ID] & GAME_SLOT_ID <= (slot_selected[, GAME_SLOT_ID] + MOVEMENT), ':=' (covered = 1, optimal = slot_selected[, optimal])]
        reverse[, counter_cons_piece := rowid(rleid(mountain_row, covered))]
        reverse[covered > 0]
        #return(reverse)
      } else {
        #does not fit. If movement> 5, fit to next biggest area

        track_area_best_fit_left  <- reverse[PIECE_ATTRIBUTE != "M" &
                                               counter_cons_piece > 5 &
                                               covered == 0,
                                             .(GAME_SLOT_ID = max(GAME_SLOT_ID)), by =  .(ascend_on_the_way = counter_cons_piece >= slots_to_ascend)]
        #check if found
        if (nrow(track_area_best_fit_left) > 0) {
          #FOUND

          track_area_best_fit_left[, optimal := ifelse(ascend_on_the_way == 0, 1, 0)]

          slot_selected <- track_area_best_fit_left[which.max(optimal)]
          reverse[GAME_SLOT_ID > slot_selected[, GAME_SLOT_ID] & GAME_SLOT_ID <= (slot_selected[, GAME_SLOT_ID] + MOVEMENT), ':=' (covered = 1, optimal = slot_selected[, optimal])]
          reverse[, counter_cons_piece := rowid(rleid(mountain_row, covered))]
          game_status <- set_cycler_position(1, slot_selected[, GAME_SLOT_ID], 1, game_status)
          reverse[zoom(game_status,10,10), on = "GAME_SLOT_ID"]

        } else {
          #not found, then we don't care about mountains. Solve the rest as bin pack problem
          rest_of_cards <-data.table(MOVEMENT = c((1:10*0 + 2), sort_cards[move_loop:length(sort_cards)]))

          valueMapTable <- data.table(MOVEMENT = c(2,3,4,5,6,7,9), value = c(1,10,100,1000,1000,1000,1000), capped_movement = c(2,3,4,5,5,5,5))
          join_map <- valueMapTable[rest_of_cards, on = "MOVEMENT"]

          reverse[, pack_problem := (rleid(covered))]
          track_to_fill <- reverse[covered == 0, .(length = .N), by = .(id = pack_problem)]
          k <- sort(track_to_fill[length >= min(rest_of_cards[, MOVEMENT]) , length])
          res <- adagio::mknapsack(join_map[,value], join_map[, capped_movement], k)
          res_data <- data.table(box = res$ksack,  join_map)
          #analyse result
          res_data[, movement_used := ifelse(box > 0, 1, 0) * capped_movement]

          needed_extra_twos <- ceiling((track_to_fill[, sum(length)] - res_data[, sum(movement_used)]) / 2)
          total_moves_used <- res_data[movement_used > 0, .N] + needed_extra_twos
          browser()
          break()
        }
      }
    }
  }
}
reverse <- fit_card(reverse, cards_to_play)
reverse[covered == 1]
}
