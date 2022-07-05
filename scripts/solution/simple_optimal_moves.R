simple_optimal_moves <- function(cycler_deck_status, calc_from_slot, precalc_data, draw_odds_raw_data = "") {




    kortit_Dt <- data.table(MOVEMENT = as.numeric(str_split(cycler_deck_status[[1]][1], "")[[1]]))




    track_left <- precalc_data[GAME_SLOT_ID == calc_from_slot, TRACK_LEFT]

    if (draw_odds_raw_data == "") {
      use_draw_odds <- ""
    } else {

      parse_draw_odds <- str_split(gsub("\\.", "", draw_odds_raw_data), ";")

      use_draw_odds <- data.table(Turn_to_Draw = as.numeric(str_split(parse_draw_odds[[1]][1], "")[[1]]),
                                  MOVEMENT =  as.numeric(str_split(parse_draw_odds[[1]][2], "")[[1]]),
                                  prob = -1)

    }




    #trRes <- tryCatch({

    #cut track
    finish_slot_first <-  precalc_data[FINISH == 1, GAME_SLOT_ID ]
    aggr_to_slots <-  precalc_data[GAME_SLOT_ID >= calc_from_slot & GAME_SLOT_ID <= finish_slot_first ]


    track_pre <- copy(aggr_to_slots)
    track_pre[, custom_slot := seq_len(.N)]
    next_ascend <- track_pre[ascend_v > 2, min(custom_slot)]
    last_move_savings <- 0
    #if next ascend is close, we need to spent card for it to hit it
    if (next_ascend <= 4) {
      need_to_earn <- TRUE
    } else {
      need_to_earn <- FALSE
    }

    #if first is not ascend, cut it. We cannot hit the first slot as max move is 2.
    first_is_ascend <- track_pre[1, ascend_v] > 2
    if (first_is_ascend == FALSE) {
      track_pre2 <- track_pre[2:nrow(track_pre)]
    } else {
      track_pre2 <- track_pre
    }
    track_pre2[, ascend_possible := ifelse(ascend_v > 2, 1, 0)]
    track_pre2[, cumulative_ascend := rowid(rleid(ascend_possible)) - 1]
    track_pre2[, new_obs := cumulative_ascend %% ascend_v]

    possible_ascends <- track_pre2[new_obs == 0  & ascend_possible == 1, .(ascend_v)]
    if (need_to_earn == TRUE) {
      possible_ascends[1, card_need_to_be_spend := next_ascend]

      if (nrow(possible_ascends) > 1) {
        easy_ascends <- possible_ascends[2:nrow(possible_ascends)]

      } else {
        easy_ascends <- possible_ascends[1 == 0]

      }
      hard_ascend <- possible_ascends[1, .(ascend_v)]
    } else {
      easy_ascends <- possible_ascends
    }
    copy_kortit <- copy(kortit_Dt)
    sort_kortit <- copy_kortit[order(MOVEMENT)][1:nrow(easy_ascends)]
    force_bind <- cbind(sort_kortit, easy_ascends)
    force_bind[, diff := max(0, ascend_v - MOVEMENT)]

    if (need_to_earn == TRUE) {
      #try to use the last too
      rest_of_cards <- copy_kortit[order(MOVEMENT)][(nrow(easy_ascends) + 1):nrow(copy_kortit)]

      #check if I can pay the first move
      rest_of_cards[, required_payment := next_ascend]
      if (nrow(rest_of_cards[required_payment == MOVEMENT]) > 0) {
        #I can pay, remove the row
        rest_of_cards[, row_id := seq_len(.N)]
        remove_id <- rest_of_cards[MOVEMENT == required_payment, min(row_id)]
        #get last used card for ascend
        last_move <- rest_of_cards[row_id != remove_id, min(MOVEMENT)]
        last_move_savings <- max(hard_ascend[, ascend_v] - last_move, 0)
      }

    }

    savings_possible <- force_bind[!is.na(diff), sum(diff)] + last_move_savings

    #total moves in deck
    moves_in_deck <- kortit_Dt[, sum(MOVEMENT)]
    trac_slots_left <- aggr_to_slots[, .N]
    moves_with_savings <- moves_in_deck + savings_possible
    moves_over_track_left <- moves_with_savings - trac_slots_left
    if (moves_over_track_left <= 1) {

      if (moves_over_track_left < 0) {
        extra_moves_needed <- ceiling(abs(moves_over_track_left) / 2)
        turns_to_left <- extra_moves_needed + nrow(kortit_Dt)
        slots_over_finish <- abs(moves_over_track_left) %% 2

      } else {
        turns_to_left <- nrow(kortit_Dt)
        slots_over_finish <- moves_over_track_left
      }
      next_move <- NA
      following_move <- NA
      trackia_leftt <- aggr_to_slots[1, TRACK_LEFT]
      turns_to_finish_res <- data.table(SAVE = FALSE, TURNS_TO_FINISH = turns_to_left, SLOTS_OVER_FINISH = slots_over_finish, NEXT_MOVE = NA,
                                        FOLLOWING_MOVE = NA, SOLUTION = NA, PRIORITY = 4,
                                        TRACK_LEFT = trackia_leftt, DRAW_ODDS = draw_odds_raw_data, DECK_LEFT = cycler_deck_status,
                                        PARENT_ID = paste0(trackia_leftt, "_", cycler_deck_status, "_", draw_odds_raw_data))

    #  print(turns_to_finish_res)

    } else {
      turns_to_finish_res <- NULL

    }
    return(turns_to_finish_res)
}
