
update_combinations_with_hidden_input <- function(combinations_data, deck_status, team_id_input,
                                                  pre_aggr_game_status, calc_ttf = 0, calc_draw_odds = FALSE) {

  #team_id_input <- 3

  copy_deck <- copy(deck_status)
  my_team <- combinations_data#[TEAM_ID == team_id_input]
  aggregate_to_cases <- my_team[, .N, by = .(DECK_LEFT, TRACK_LEFT, NEW_GAME_SLOT_ID, CYCLER_ID, TEAM_ID, MOVEMENT)]
  #DRAW ODDS DISABLED FOR PERFORMANCE
  if (calc_draw_odds == TRUE) {

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
  } else {
    aggregate_to_cases[, DRAW_ODDS := ""]
  }
##############

  aggregate_to_cases_after_calc_with_MOVEMENT <- aggregate_to_cases[, .N, by = .(DECK_LEFT, TRACK_LEFT, DRAW_ODDS, NEW_GAME_SLOT_ID, TEAM_ID, CYCLER_ID)]

  aggr_opt_moves <- ADM_OPTIMAL_MOVES[, .(TRACK_LEFT = TRACK_LEFT[which.min(PRIORITY)],
                                          DECK_LEFT  = DECK_LEFT [which.min(PRIORITY)],
                                          TURNS_TO_FINISH  = TURNS_TO_FINISH [which.min(PRIORITY)],
                                          DRAW_ODDS  = DRAW_ODDS [which.min(PRIORITY)],
                                          SLOTS_OVER_FINISH = SLOTS_OVER_FINISH[which.min(PRIORITY)],
                                          NEXT_MOVE = NEXT_MOVE[which.min(PRIORITY)]),
                                      by = .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS)]


  join_known_cases <- aggr_opt_moves[aggregate_to_cases_after_calc_with_MOVEMENT, on = .(DECK_LEFT, TRACK_LEFT, DRAW_ODDS)]
  #print("MISSING CASES")
  count_ss_ttf <- join_known_cases[is.na(TURNS_TO_FINISH), .N]
  count_cases <- join_known_cases[, .N]
  coverage <- 1 - count_ss_ttf / count_cases

print("coverage and missing")
print(coverage)
print(count_ss_ttf)
    alku <- Sys.time()
  #  print("lasketaan")
#calculating
 #count missing cases
    join_known_cases[, IS_FINISHED := ifelse(TRACK_LEFT %in% c("", "N"), 1, 0)]


        added_ttf <- add_ttf_multicore(con, join_known_cases[is.na(TURNS_TO_FINISH) & IS_FINISHED == 0], pre_aggr_game_status)

   # added_ttf <- add_ttf_multicore(con, join_known_cases[50], pre_aggr_game_status)

    join_known_cases <- update_dt_values(join_known_cases, added_ttf, c("DRAW_ODDS", "TRACK_LEFT", "DECK_LEFT"),
                                   c("TURNS_TO_FINISH", "SLOTS_OVER_FINISH", "NEXT_MOVE"))
    finish_slot <- pre_aggr_game_status[FINISH == 1, GAME_SLOT_ID]
    join_known_cases[is.na(TURNS_TO_FINISH) & IS_FINISHED == 1, ':=' (TURNS_TO_FINISH = 0, SLOTS_OVER_FINISH = NEW_GAME_SLOT_ID - finish_slot, NEXT_MOVE = 5)]
  result <- join_known_cases[, .N, by = .(TTF_HIDDEN = TURNS_TO_FINISH, TRACK_LEFT, DECK_LEFT, NEW_GAME_SLOT_ID, TEAM_ID, CYCLER_ID )]
  joinBack <- result[my_team, on = .(TRACK_LEFT, DECK_LEFT, NEW_GAME_SLOT_ID, TEAM_ID, CYCLER_ID)]
  joinBack[, ':=' (TURNS_TO_FINISH = TTF_HIDDEN,
                   TTF_HIDDEN = NULL,
                   N = NULL)]
  return(joinBack)

}
