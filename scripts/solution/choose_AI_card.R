choose_AI_card <- function(cycler_id, game_status, deck_status, action_data1, precalc_track_res) {

  my_hand <- deck_status[CYCLER_ID == cycler_id & Zone == "Hand"]
  best_move <- NULL
  tot_analysis <- data.table(Setting = "")
  # orig_speed <- turns_to_finish(game_status, deck_status)
  for (best_loop in my_hand[, .N, by = MOVEMENT][, MOVEMENT]) {
    action_data1[CYCLER_ID == cycler_id, MOVEMENT := best_loop]
    pos_score <- suppressWarnings(score_position(game_status, deck_status, action_data1, ADM_AI_CONF, orig_speed, precalc_track_res, cycler_id))[!is.na(Score)]
    my_score <- pos_score[CYCLER_ID == cycler_id, sum(Result)]
    #print(paste0("MY ", my_score))
    my_analysis <- pos_score[CYCLER_ID == cycler_id, sum(Result), by = Setting]
    setnames(my_analysis, "V1", paste0("M",best_loop))
    tot_analysis <- tot_analysis[my_analysis, on = "Setting"]
    # print( pos_score[CYCLER_ID == cycler_id])
    #kovin_vrihu <- AI(cycler_id = cycler_id, game_status, deck_status)
    enemy_score <- pos_score[CYCLER_ID != cycler_id, sum(Result), by = CYCLER_ID][, mean(V1)]
    #print(paste0("ENEM ", enemy_score))
    # print( pos_score[CYCLER_ID != cycler_id, sum(Result), by = Setting])
    # print(tot_analysis)
    row_data <- data.table(tot_score = my_score - 0, MOVEMENT = best_loop)
    best_move <- rbind(row_data, best_move)
    #print(pos_score[CYCLER_ID == cycler_id])
  }

  #  print(tot_analysis)
  selected_move <- best_move[ , .SD[which.max(tot_score)]][, MOVEMENT]
  print(zoom(game_status))
  print(paste0(selected_move, " - ", paste0( my_hand[, .N, by = MOVEMENT][, MOVEMENT], collapse = ",")))


  result_row <- data.table(CYCLER_ID = cycler_id, MOVEMENT = selected_move, CARD_ID = selected_move)
  return(result_row)
}
