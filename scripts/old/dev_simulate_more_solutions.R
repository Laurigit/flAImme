simulate_more_solutions <- function(ttf_results, n_steps, pre_agg_no_list) {

  solution_raw <- "2,4,3,9,5,3,5"
  soluton_vec <- as.numeric(unlist(str_split(solution_raw, ",")))
  "111;239"  "95543322"
  track_left <- "SSSSNNNNNNNNNNNMMMAAANNNNNNNN"
  #move n steps
  #n_steps <- 3
  #copy_game <- copy(game_status)
  curr_slot <-  track_status[, GAME_SLOT_ID]
  used_cards <- NULL
  track_status <- pre_agg_no_list[TRACK_LEFT == track_left]
  for (i in 1:n_steps) {

    max_move <- track_status[, MAXIMUM_MOVEMENT]
    min_move <- track_status[, MINIMUM_MOVEMENT]
    used_cards <- c(soluton_vec[i], used_cards)

    curr_slot <- min(max(min_move, soluton_vec[i]), max_move) + curr_slot
    track_status <- pre_agg_no_list[GAME_SLOT_ID == curr_slot]
  }

}
