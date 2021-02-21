
calculate_missing_TTF <- function()
  only_my_ttf <-   own_TTF_data[, .(MY_TURNS_TO_FINISH, CYCLER_ID, MOVEMENT, new_slot_after_moving)]

  join_my_ttf <- only_my_ttf[combinations_with_p, on = .(CYCLER_ID, MOVEMENT, new_slot_after_moving == NEW_GAME_SLOT_ID)]


  thinking_time_so_far <- difftime(Sys.time(), turn_start_time, units = c("secs"))
  draw_odds_and_deck_left <-   own_TTF_data[, .(CYCLER_ID, MOVEMENT, DRAW_ODDS, DECK_LEFT)]
  join_draw_odds <- draw_odds_and_deck_left[join_my_ttf, on = .(CYCLER_ID, MOVEMENT)]
  count_missing <- join_draw_odds[is.na(MY_TURNS_TO_FINISH) & CYCLER_ID %in% smart_cycler_ids][, .N]
  # if (as.numeric(thinking_time_so_far) <= 60 & count_missing > 0) {
  if ( count_missing > 0) {

    join_draw_odds[is.na(MY_TURNS_TO_FINISH) & CYCLER_ID %in% smart_cycler_ids,
                   recalc_ttf := as.double(finish_turns_db(con, TRACK_LEFT, DECK_LEFT, pre_aggr_game_status, new_slot_after_moving,
                                                           draw_odds_raw_data = DRAW_ODDS, save_to_DB = TRUE)),
                   by = .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS, new_slot_after_moving)]
    join_draw_odds[is.na(MY_TURNS_TO_FINISH), MY_TURNS_TO_FINISH := recalc_ttf]
    join_draw_odds[!is.na(MY_TURNS_TO_FINISH) %in% smart_cycler_ids]
    join_my_ttf <- copy(join_draw_odds)
    join_my_ttf[, TURNS_TO_FINISH := ifelse(!is.na(MY_TURNS_TO_FINISH), MY_TURNS_TO_FINISH, TURNS_TO_FINISH)]
    setnames(join_my_ttf, "new_slot_after_moving", "NEW_GAME_SLOT_ID")
  }
