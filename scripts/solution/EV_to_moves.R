
EV_to_moves <- function(EV_table, deck_status) {



  check_res2 <- EV_table
  check_res2[, M1 := as.numeric(str_sub(MOVES, 1, 1))]
  check_res2[, M2 := as.numeric(str_sub(MOVES, 3, 3))]

  check_res2[, C1 := as.numeric(word(CYCLERS, 1, 1, sep = fixed("_")))]
  check_res2[, C2 := as.numeric(word(CYCLERS, 2, 2, sep = fixed("_")))]
  two_cyclers <- check_res2[!is.na(C2), .N]
  if (two_cyclers > 0) {



  check_res2[,  DECK_LEFT_C1 := convert_deck_left_to_text(deck_status, C1), by = .(MOVES )]
  check_res2[,  DECK_LEFT_C2 := convert_deck_left_to_text(deck_status, C2), by = .(MOVES )]
  check_res2[, draw_odds_C1 := calculate_odds_of_drawing_card(DECK_LEFT_C1, M1)]
  check_res2[, draw_odds_C2 := calculate_odds_of_drawing_card(DECK_LEFT_C2, M2)]


  worse_move1 <- check_res2[, .(MOVEMENT = as.numeric(M1), CYCLER_ID = as.numeric(C1), OTHER_MOVE = as.numeric(M2), OTHER_CYCLER = as.numeric(C2), TEAM_ID, EV, TURNS_TO_FINISH)]
  worse_move2 <- check_res2[, .(MOVEMENT = as.numeric(M2), CYCLER_ID = as.numeric(C2), OTHER_MOVE = as.numeric(M1),  OTHER_CYCLER = as.numeric(C1),TEAM_ID, EV, TURNS_TO_FINISH)]
  append_worse <- rbind(worse_move1, worse_move2)
  sorted_app <- append_worse[order(CYCLER_ID, MOVEMENT, -EV)]
  sorted_app[, PRIO := seq_len(.N), by = .(CYCLER_ID, MOVEMENT)]
  # append_worse[CYCLER_ID == 5 & MOVEMENT == 7]
  sorted_app[, playing_prob_other := calculate_draw_distribution_by_turn_with_prio(.SD, deck_status), by = .(CYCLER_ID, MOVEMENT), .SDcols = c("OTHER_CYCLER", "OTHER_MOVE", "PRIO")]

  EVs <- sorted_app[, .(WEIGHTED_EV = sum(EV * playing_prob_other), TURNS_TO_FINISH = sum(TURNS_TO_FINISH *playing_prob_other)), by = .(MOVEMENT, CYCLER_ID)]
  #copy columns to fit to function
  setorder(EVs, CYCLER_ID, -WEIGHTED_EV)
  EVs[, ':=' (PRIO = seq_len(.N), OTHER_CYCLER = CYCLER_ID, OTHER_MOVE = MOVEMENT), by = CYCLER_ID]
  EVs[, playing_prob_me := calculate_draw_distribution_by_turn_with_prio(.SD, deck_status), by = .(CYCLER_ID), .SDcols = c("OTHER_CYCLER", "OTHER_MOVE", "PRIO")]

  EV_OF_PLAYING_FIRST <- EVs[, sum(WEIGHTED_EV * playing_prob_me), by = CYCLER_ID]


  first_selected_cycler <- EV_OF_PLAYING_FIRST[which.max(V1), CYCLER_ID]
  #    p1_data <- check_res2[, ]




  options <- deck_status[CYCLER_ID == first_selected_cycler & Zone == "Hand", .N, by = .(MOVEMENT)][, MOVEMENT]
  cards_in_hand <- EVs[CYCLER_ID == first_selected_cycler & MOVEMENT %in% options]

  first_movement <- cards_in_hand[, MOVEMENT[which.max(WEIGHTED_EV)]]
  #convert exhaust if possible
  first_card_id <- deck_status[CYCLER_ID == first_selected_cycler & Zone == "Hand" & MOVEMENT == first_movement, min(CARD_ID)]

  second_cycler_in_team <- EV_OF_PLAYING_FIRST[!CYCLER_ID %in% first_selected_cycler, .(CYCLER_ID)]
  #still playing?
  #  second_cycler <- intersect(cycler_ids, second_cycler_in_team)
  if (nrow(second_cycler_in_team) == 1) {
    second_cycler <- second_cycler_in_team[, CYCLER_ID]
    # card_id_2nd  <-  card_selector_by_stat(game_status, deck_status[CYCLER_ID == second_cycler & Zone == "Hand"], second_cycler, "SMART_MAX",  aim_downhill = TRUE)[, CARD_ID]
    options <- deck_status[CYCLER_ID == second_cycler & Zone == "Hand", .N, by = MOVEMENT][, MOVEMENT]
    cards_in_hand <- append_worse[CYCLER_ID == second_cycler & OTHER_MOVE == first_movement & MOVEMENT %in% options]




    #  print(cards_in_hand)
    second_movement <-  cards_in_hand[, MOVEMENT [which.max(EV)]]
    second_card_id <- deck_status[CYCLER_ID == second_cycler & Zone == "Hand" & MOVEMENT == second_movement, min(CARD_ID)]

  }

  result <- data.table(FIRST = c(TRUE, FALSE), CYCLER_ID = c(first_selected_cycler, second_cycler),
                       CARD_ID = c(first_card_id, second_card_id))
  } else{
    cycler <- check_res2[which.max(EV), C1]
    options <- deck_status[CYCLER_ID == cycler & Zone == "Hand", .N, by = .(MOVEMENT)][, MOVEMENT]


    card <- check_res2[MOVES  %in% options][which.max(EV), M1]

    result <- data.table(FIRST = TRUE, CYCLER_ID = cycler,
                         CARD_ID = card)
  }
return(result)

}
