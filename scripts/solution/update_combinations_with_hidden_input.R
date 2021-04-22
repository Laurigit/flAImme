
update_combinations_with_hidden_input <- function(combinations_data, deck_status, team_id_input, pre_aggr_game_status, calc_ttf = 0) {

  #team_id_input <- 3

  copy_deck <- copy(deck_status)
  my_team <- combinations_data#[TEAM_ID == team_id_input]
  aggregate_to_cases <- my_team[, .N, by = .(DECK_LEFT, TRACK_LEFT, NEW_GAME_SLOT_ID, CYCLER_ID, TEAM_ID, MOVEMENT)]
  aggregate_to_cases[, DRAW_ODDS := ifelse(TEAM_ID == team_id_input, calculate_draw_distribution_by_turn(CYCLER_ID, play_card(CYCLER_ID,
                                                                                                                              card_id = NULL,
                                                                                                                              copy_deck,
                                                                                                                              game_id = 0,
                                                                                                                              turn_id = 0,
                                                                                                                              con = NULL,
                                                                                                                              card_row_id = FALSE,
                                                                                                                              MOVEMENT_PLAYED = MOVEMENT,
                                                                                                                              force = TRUE,
                                                                                                                              copy = TRUE), how_many_cards = 4, db_res = TRUE), ""),
                     by = .(DECK_LEFT, TRACK_LEFT, NEW_GAME_SLOT_ID, CYCLER_ID, MOVEMENT)]




  aggregate_to_cases_after_calc_with_MOVEMENT <- aggregate_to_cases[, .N, by = .(DECK_LEFT, TRACK_LEFT, DRAW_ODDS, NEW_GAME_SLOT_ID, TEAM_ID, CYCLER_ID)]
  join_known_cases <- ADM_OPTIMAL_MOVES[aggregate_to_cases_after_calc_with_MOVEMENT, on = .(DECK_LEFT, TRACK_LEFT, DRAW_ODDS)]
  print("MISSING CASES")
  count_ss_ttf <- join_known_cases[is.na(TURNS_TO_FINISH), .N]
  count_cases <- join_known_cases[, .N]
  coverage <- 1 - count_ss_ttf / count_cases
  print("coverage ad missing")
  print(coverage)
  print(count_ss_ttf)
  if ((16 - calc_ttf) * count_ss_ttf < 400) {
    alku <- Sys.time()
    print("lasketaan")
#calculating
 #count missing cases


  join_known_cases[is.na(TURNS_TO_FINISH), c("TURNS_TO_FINISH", "SLOTS_OVER_FINISH", "NEXT_MOVE") := (finish_turns_db(con, TRACK_LEFT, DECK_LEFT, pre_aggr_game_status, NEW_GAME_SLOT_ID,
                                                                                   draw_odds_raw_data = DRAW_ODDS, save_to_DB = TRUE)),
                  by = .(NEW_GAME_SLOT_ID, TRACK_LEFT, DECK_LEFT, DRAW_ODDS)]
  print("keso")
  print(difftime(Sys.time(), alku, units = c("secs")))
} else {
  print("ei laskeat")
  join_known_cases <- aggregate_to_cases_after_calc_with_MOVEMENT
  join_known_cases[, ':=' (TURNS_TO_FINISH = 15 - calc_ttf, SLOTS_OVER_FINISH = 1, NEXT_MOVE = 0)]

}
  result <- join_known_cases[, .N, by = .(TTF_HIDDEN = TURNS_TO_FINISH, TRACK_LEFT, DECK_LEFT, NEW_GAME_SLOT_ID, TEAM_ID, CYCLER_ID )]
  joinBack <- result[my_team, on = .(TRACK_LEFT, DECK_LEFT, NEW_GAME_SLOT_ID, TEAM_ID, CYCLER_ID)]
  joinBack[, ':=' (TURNS_TO_FINISH = TTF_HIDDEN,
                   TTF_HIDDEN = NULL,
                   N = NULL)]
  return(joinBack)

}
