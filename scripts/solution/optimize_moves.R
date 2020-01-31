optimize_moves <- function(game_status, deck_status, cycler_ids){

  cycler_ids <- game_status[CYCLER_ID > 0, CYCLER_ID]
  cycler_id <- 1
  cards_to_play <- deck_status[CYCLER_ID == cycler_id, MOVEMENT]


  aggr_to_slots <- game_status[is.na(EXTRA) & GAME_SLOT_ID <= game_status[FINISH == 1, GAME_SLOT_ID], .N, by = .(GAME_SLOT_ID, PIECE_ATTRIBUTE, FINISH)]


  aggr_to_slots[, mountain_row := ifelse(PIECE_ATTRIBUTE == "M", 1, 0)]
  aggr_to_slots[, ascend_row := ifelse(PIECE_ATTRIBUTE == "A", 1, 0)]
  reverse <- aggr_to_slots[order(-GAME_SLOT_ID)]
 # reverse[, start_of_restricted_movement := ifelse(shift(mountain_row == 1, n = 6) | mountain_row == 1, 1, 0)]
 # reverse[, start_of_restricted_movement := ifelse(is.na(start_of_restricted_movement), 0, start_of_restricted_movement)]
  reverse[, counter_cons_piece := rowid(rleid(mountain_row))]
  reverse[, slots_to_ascend := rowid(rleid(ascend_row))]
  reverse[, covered := 0]
  reverse[, optimal := -1]

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
