
update_combinations_with_hidden_input <- function(combinations_data, deck_status, team_id_input, pre_aggr_game_status) {

  #team_id_input <- 3
  copy_deck <- copy(deck_status)
  my_team <- combinations_data <- combinations_data#[TEAM_ID == team_id_input]
  aggregate_to_cases <- my_team[, .N, by = .(DECK_LEFT, TRACK_LEFT, NEW_GAME_SLOT_ID, CYCLER_ID, TEAM_ID)]
  aggregate_to_cases[, DRAW_ODDS := ifelse(TEAM_ID == team_id_input, calculate_draw_distribution_by_turn(CYCLER_ID, deck_status, how_many_cards = 4, db_res = TRUE), ""),
                     by = .(DECK_LEFT, TRACK_LEFT, NEW_GAME_SLOT_ID, CYCLER_ID)]

  rm(ADM_OPTIMAL_MOVES, envir = globalenv())
  required_data("ADM_OPTIMAL_MOVES")
  join_known_cases <- ADM_OPTIMAL_MOVES[aggregate_to_cases, on = .(DECK_LEFT, TRACK_LEFT, DRAW_ODDS)]

  join_known_cases[is.na(TURNS_TO_FINISH), c("TURNS_TO_FINISH", "SLOTS_OVER_FINISH", "NEXT_MOVE") := (finish_turns_db(con, TRACK_LEFT, DECK_LEFT, pre_aggr_game_status, NEW_GAME_SLOT_ID,
                                                                                   draw_odds_raw_data = DRAW_ODDS, save_to_DB = TRUE)),
                  by = .(NEW_GAME_SLOT_ID, TRACK_LEFT, DECK_LEFT, DRAW_ODDS)]

  result <- join_known_cases[, .N, by = .(TTF_HIDDEN = TURNS_TO_FINISH, TRACK_LEFT, DECK_LEFT, NEW_GAME_SLOT_ID, TEAM_ID )]
  joinBack <- result[my_team, on = .(TRACK_LEFT, DECK_LEFT, NEW_GAME_SLOT_ID, TEAM_ID)]
  joinBack[, ':=' (TURNS_TO_FINISH = TTF_HIDDEN,
                   TTF_HIDDEN = NULL,
                   N = NULL)]
  return(joinBack)

}
