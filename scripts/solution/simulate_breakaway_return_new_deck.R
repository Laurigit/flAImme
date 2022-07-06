simulate_breakaway_return_new_deck <- function(game_status, deck_status, bet_limit) {
  cp_deck <- copy(deck_status)
  cp_team <- copy(game_status)
  cp_team[, team_temp := ceiling(CYCLER_ID / 2)]
  # worst_cycler <- cp_team[team_temp > 0, .(CYCLER_ID = CYCLER_ID[which.min(SQUARE_ID)],
  #                                 GAME_SLOT_ID = GAME_SLOT_ID[which.min(SQUARE_ID)]), by = team_temp]
  worst_cycler <- cp_team[team_temp > 0, .(CYCLER_ID, GAME_SLOT_ID)]

  random_two <- cp_deck[, .SD[sample(x = .N, size = 2)], by = CYCLER_ID]
  print(random_two)
  sum_cards <- random_two[, .(TOT_BET = sum(MOVEMENT)), by = CYCLER_ID]
  joib_bet <- sum_cards[worst_cycler, on = "CYCLER_ID"]
  joib_bet[, TOTAL_BENEFIT_IN_START := 10 - GAME_SLOT_ID]
  joib_bet[, OVER_BET :=   TOT_BET - TOTAL_BENEFIT_IN_START]
  joib_bet[, OVER_BET_PENALTY := ifelse(OVER_BET > bet_limit, -1000  * (OVER_BET - bet_limit), 0)]
  joib_bet[, FINAL_KPI := OVER_BET + OVER_BET_PENALTY]
  #choose two cyclers who have max TOT_BET but under limit
  joib_bet[, team_temp := ceiling(CYCLER_ID / 2)]
    best_bet_in_team <- joib_bet[joib_bet[, .I[which.max(FINAL_KPI)], by=team_temp]$V1]
  winners <- best_bet_in_team[order(-FINAL_KPI)][1:2, CYCLER_ID]

  card_ids_to_remove <- random_two[CYCLER_ID %in% winners, row_id]
  cp_deck[row_id %in% card_ids_to_remove, Zone := "Removed"]

  for (winner_loop in winners) {

    cp_deck <-  add_exhaustion(winner_loop, cp_deck, "Deck")
    cp_deck <- add_exhaustion(winner_loop, cp_deck, "Deck")
  }
  return(cp_deck)
}
