smart_cards_options <- function(card_options, pre_aggr_game_status, cycler_id) {

  #pre_aggr_game_status$cycler_pos[CYCLER_ID == 8, cycler_pos :=8]
#join track info


track_info <- pre_aggr_game_status$aggr_to_slots[pre_aggr_game_status$cycler_pos, on = .(GAME_SLOT_ID = cycler_pos),. (CYCLER_ID, MAXIMUM_MOVEMENT, ascend_v)]
me_cycler <- track_info[CYCLER_ID == cycler_id]

#check ascend


#remove all cards higher than max movement, expect if we dont have exactly max movement card, then include smallest that is higher
max_move_var <- me_cycler[, MAXIMUM_MOVEMENT]
temp_res <- card_options[card_options <= max_move_var]

if (max(temp_res) < max_move_var) {
  temp_res <- c(temp_res, min(card_options[card_options > max_move_var]))
}


  #downhill, # options are higher than min move and smallest equal or smaller
  min_card <- min(temp_res)
  temp_res <- temp_res[(temp_res == min_card | temp_res > me_cycler[, ascend_v])]




temp_res <- temp_res[is.finite(temp_res)]
return(temp_res)


}

