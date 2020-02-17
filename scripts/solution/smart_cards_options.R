smart_cards_options <- function(card_options, pre_aggr_game_status, cycler_id) {


#join track info



track_info <- pre_aggr_game_status$aggr_to_slots[pre_aggr_game_status$cycler_pos, on = .(GAME_SLOT_ID = cycler_pos),. (CYCLER_ID, restricted_v, ascend_v)]
me_cycler <- track_info[CYCLER_ID == cycler_id]
card_options_res <- card_options
#check ascend
if (me_cycler[, restricted_v]) {
  #uphill, check if we have any 5 or lower
  min_card <- min(card_options)
  #if 5 or higher, then only options is to play smalles card
  if (min_card >= 5) {
    card_options_res <- min_card
  } else {
    card_options_res <- card_options[card_options <= 5]
  }

}


if (me_cycler[, ascend_v]) {
  #downhill,
  min_card <- min(card_options)
  card_options_res <- card_options[card_options == min_card | card_options > 5]

}
return(card_options_res)


}

