con <- connDB(con, dbname_input = "flaimme")
rm("ADM_OPTIMAL_MOVES")
required_data(c("STG_TRACK", "STG_TRACK_PIECE", "ADM_CYCLER_DECK", "ADM_OPTIMAL_MOVES"))
required_data("ADM_AI_CONF")
track <- 2
cyclers <- c(1,2,3,4,5,6)
game_status <- start_game(cyclers,track, STG_TRACK_PIECE, STG_TRACK)
#game_status <- set_cycler_position(1, 64, 1,  game_status)
deck_status <- create_decks(cyclers, ADM_CYCLER_DECK)
zoom(game_status)


ijk <- ijk_map(track, STG_TRACK_PIECE, STG_TRACK)

#played_cards <- data.table(cards = c(4, 4, 5, 5))

slots_squares <- as.matrix(game_status[, .(SQUARE_ID, GAME_SLOT_ID)])
reverse_slots_squares <- slots_squares[nrow(slots_squares):1,]
#



#everybody draws
for(loop in cyclers) {
  deck_status <- draw_cards(loop, deck_status)
  # print(list(deck_status[CYCLER_ID == loop & Zone == "Hand", CARD_ID]))

}


pre_track <- precalc_track(game_status)
ctM_res <- cyclers_turns_MOVEMEMENT_combs(con, ADM_OPTIMAL_MOVES, game_status, deck_status, pre_track)
ctM_data <- ctM_res$ctM_data
smart_cyclers <- c(5, 6)
moving_cycler <- 5

range <- calc_move_range(game_status, deck_status, ctM_data, STG_TEAM)
team_id <- 3
phase <- 1

card_options_in_hand <- smart_cards_options(deck_status[CYCLER_ID == moving_cycler & Zone == "Hand", unique(MOVEMENT)], pre_track, moving_cycler)
if (length(card_options_in_hand) == 1) {
  res_move <- data.table(MOVEMENT = card_options_in_hand, CYCLER_ID = moving_cycler, OPTION = TRUE)
} else {

  range <- suppressWarnings(calc_move_range(game_status, deck_status, ctM_data, STG_CYCLER))
  if (phase == 2) {
    phase_two_cyclers<-  deck_status[Zone == "Hand", .N, by = CYCLER_ID][, CYCLER_ID]
    p2_score_outout <- phase2_slot_score(game_status, phase_two_cyclers, deck_status, range_joined_team, pre_aggr_game_status)
    range <- suppressWarnings(calc_move_range_phase_2(game_status, deck_status, ctM_data, p2_score_outout, STG_CYCLER))
  }



  cyclers <- game_status[CYCLER_ID > 0, CYCLER_ID]

  #pre_track <- precalc_track(game_status)
  #ctM_res <- cyclers_turns_MOVEMEMENT_combs(con, ADM_OPTIMAL_MOVES, game_status, deck_status, pre_track)


  sscols <- range[, .(TEAM_ID, CYCLER_ID, MOVEMENT)]
  #eventhough I know what cards I have, others dont know it.
  #sscols <- res_move[sscols_all, on = .(CYCLER_ID, MOVEMENT)][OPTION == TRUE | CYCLER_ID != moving_cycler]

  aggr <- sscols[, .N, by = CYCLER_ID]
  poss_combs <- prod(aggr[, N])
  cyclers <- sscols[, .N, by = CYCLER_ID][, CYCLER_ID]
  tot_data <- data.table(del_me = "")
  for (cyc_loop in cyclers) {
    tot_data <- CJ.dt(tot_data, sscols[CYCLER_ID == cyc_loop, .(CYCLER_ID, odds = 0,  MOVEMENT)])

  }
  tot_data[, del_me := NULL]
  appendloop <-  tot_data[, 1:3, with = FALSE]
  setnames(appendloop, colnames(appendloop), c("CYCLER_ID", "odds", "MOVEMENT"))
  appendloop[, case_id := seq_len(.N)]
  tot_data[, (1:3) := NULL]
  for (appendCOunt in 1:(length(cyclers) - 1)) {

    #tot_data[, del_me := NULL]
    splitcol <- tot_data[, 1:3, with = FALSE]
    setnames(splitcol, colnames(splitcol), c("CYCLER_ID", "odds", "MOVEMENT"))
    splitcol[, case_id := seq_len(.N)]

    appendloop <- rbind(appendloop, splitcol)
    tot_data[, (1:3) := NULL]
  }
  filter_zero <- appendloop[order(case_id, CYCLER_ID)]


  #CALC DRAW ODDS HERE AS IT DEPENDS ON PLAYED CARD! but it only calculated later as opponents donw know my odds
  # range[, DRAW_ODDS :=  ifelse(CYCLER_ID %in% smart_cyclers, (calculate_draw_distribution_by_turn(CYCLER_ID,
  #                                                                                                 play_card(CYCLER_ID,
  #                                                                                                           card_id = NULL,
  #                                                                                                           deck_status,
  #                                                                                                           game_id = 0,
  #                                                                                                           turn_id = 0,
  #                                                                                                           con = NULL,
  #                                                                                                           card_row_id = NULL,
  #                                                                                                           MOVEMENT_PLAYED = MOVEMENT,
  #                                                                                                           copy = TRUE),
  #                                                                                                 4, db_res = TRUE)),
  #                              ""), by = .(CYCLER_ID, MOVEMENT)]

  #join deck left
  cycler_movement <- range[, .(CYCLER_ID, MOVEMENT, DECK_LEFT, DRAW_ODDS)]
  join_deck_left <- cycler_movement[filter_zero, on = .(CYCLER_ID, MOVEMENT)]



  #curr pos
  currpos <- game_status[CYCLER_ID > 0, .(CYCLER_ID, GAME_SLOT_ID, SQUARE_ID)]
  #add_drow_odds here

  #calculate draw odds for smart cyclers
  #jionteam
  currpos_team <- STG_CYCLER[currpos, on = "CYCLER_ID"][, .(CYCLER_ID, TEAM_ID, curr_pos = GAME_SLOT_ID, curr_square = SQUARE_ID)]
  #join tosimul
  join_info_all <- currpos_team[join_deck_left, on = "CYCLER_ID"]


  #aggr_odds <- join_info[case_id == 1, .(case_odds = prod(odds), cyc_vec = list(phaseless_simulation(game_status, 5, 6, 3, STG_CYCLER, .SD, TRUE))), by = case_id, .SDcols = c("MOVEMENT", "CYCLER_ID", "curr_pos", "TEAM_ID")]
  join_info_all[, case_odds := prod(odds), by = case_id]
  join_info_all[, case_odds_ranking_total := frank(-case_odds, ties.method = c("min"))]
  join_info <- join_info_all[order(-case_odds)]

  #join_info[, cycler_odds_rank := frank(-case_odds), by = .(CYCLER_ID, MOVEMENT)]
  #case_odds < 30

  max_new_cyc_pos <- join_info[, max(curr_pos, na.rm = TRUE)]
  if (phase == 1) {

    #thinking_time <- 2
    browser()
    setorder(join_info, case_id, -curr_square)
    reverse_slots_squares

    matr_ijk <- as.matrix(ijk)
    join_info[, c("cyc_id_copy","MOVE_DIFF", "NEW_GAME_SLOT_ID", "EXHAUST", "NEW_SQUARE") := as.data.table(move_cycler_c(as.matrix(.SD), matr_ijk, reverse_slots_squares)),
              by = .(case_id), .SDcols = c("CYCLER_ID", "MOVEMENT", "curr_pos")]

    track_ss <- pre_track$aggr_to_slots[, .(NEW_GAME_SLOT_ID = GAME_SLOT_ID, TRACK_LEFT)]
    join_track_left <- track_ss[join_info, on = .(NEW_GAME_SLOT_ID)]
    required_data("ADM_OPTIMAL_MOVES")
    join_track_left[!is.na(TRACK_LEFT), TURNS_TO_FINISH := as.double(finish_turns_db(con, TRACK_LEFT, DECK_LEFT, pre_track, NEW_GAME_SLOT_ID,
                                                                                     draw_odds_raw_data = DRAW_ODDS, save_to_DB = TRUE)),
                    by = .(NEW_GAME_SLOT_ID, TRACK_LEFT, DECK_LEFT, DRAW_ODDS)]





  #join track
  track_ss <- pre_track$aggr_to_slots[, .(new_cyc_pos = GAME_SLOT_ID, TRACK_LEFT)]
  join_track_left <- track_ss[join_info, on = .(new_cyc_pos)]
  join_track_left[!is.na(TRACK_LEFT), TURNS_TO_FINISH := as.double(finish_turns_db(con, TRACK_LEFT, DECK_LEFT, pre_track, new_cyc_pos,
                                                                                   draw_odds_raw_data = DRAW_ODDS, save_to_DB = TRUE)),
                  by = .(new_cyc_pos, TRACK_LEFT, DECK_LEFT, DRAW_ODDS)]
  #now we score, then guess opponents moves, then we add draw odds, then we choose our best move


  #add draw_odds

  onlycalced <- join_track_left[!is.na(TURNS_TO_FINISH)]

  rm(ADM_OPTIMAL_MOVES)
  required_data("ADM_OPTIMAL_MOVES")


  #start creating KPIs

  #TTS
  onlycalced[, leader_turns :=  min(TURNS_TO_FINISH), by = case_id]
  onlycalced[, second_turns := min(ifelse(TURNS_TO_FINISH > leader_turns, TURNS_TO_FINISH, 1000))]
  onlycalced[, is_leader := TURNS_TO_FINISH == leader_turns]
  onlycalced[, my_min_ttf := min(TURNS_TO_FINISH), by = .(CYCLER_ID, case_id)]
  onlycalced[, ttf_score :=  my_min_ttf - TURNS_TO_FINISH  + as.numeric(is_leader) * 0.5, by = .(CYCLER_ID, case_id)]

  #EXHAUST
  onlycalced[, exhaust_free_slots := list(list(new_cyc_pos - 1)), by = case_id]
  onlycalced[, exhaust_added := !new_cyc_pos %in% exhaust_free_slots[[1]], by = .(CYCLER_ID, case_id)]
  onlycalced[, exhaust_score := -exhaust_added * TURNS_TO_FINISH / 15 ]

  #CYCLER_DISTANCE
  onlycalced[, team_penalty_score := (pmax(mean(new_cyc_pos) - new_cyc_pos, 1) ^ (1 / 2) - 1) / 2, by = .(TEAM_ID, case_id)]

  #FINISH SCORE
  finish_slot <- pre_track$aggr_to_slots[FINISH == 1, GAME_SLOT_ID]
  onlycalced[, over_finish_square := max(new_cyc_pos - finish_slot, 0) * MOVEMENT * 100]

  onlycalced[, actual_movement := (new_cyc_pos - curr_pos)]
  onlycalced[, move_diff := actual_movement - MOVEMENT]
  #calculate score

  cyc_dist_eff <- ADM_AI_CONF[CYCLER_ID == 6  & Setting == "Cycler_distance", Value]
  exh_eff <- ADM_AI_CONF[CYCLER_ID == 6 & Setting == "Get_exhaust", Value]
  speed_eff <- ADM_AI_CONF[CYCLER_ID == 6 & Setting == "Speed_change", Value]
  move_gai_eff <- ADM_AI_CONF[CYCLER_ID == 6 & Setting == "Movement_gained", Value]
  card_spend_eff <-  ADM_AI_CONF[CYCLER_ID == 6 & Setting == "Card_spent", Value]



  onlycalced[, row_id := seq_len(.N)]
  onlycalced[, Result :=
               over_finish_square +
               exhaust_score * exh_eff +
               ttf_score * speed_eff +
               actual_movement * move_gai_eff * 1.1+
               -MOVEMENT * card_spend_eff
             ,by = .(case_id, CYCLER_ID)]

  sscols_res <- onlycalced[, .(case_id, CYCLER_ID, TEAM_ID, MOVEMENT, Result, case_odds, ttf_score, exhaust_score, team_penalty_score, move_diff)]

  aggre <- sscols_res[CYCLER_ID %in% c(moving_cycler, second_cycler), .(team_Result = mean(Result)), by = .( case_id)]#[order(CYCLER_ID, MOVEMENT)]

  join_team_res <- aggre[sscols_res, on = "case_id"]
  choose_move <- join_team_res[CYCLER_ID == moving_cycler, mean(team_Result), by = MOVEMENT]
  max_score <- choose_move[, max(V1)]


  res_move <- max(choose_move[V1 == max_score, MOVEMENT])
}
return(res_move)
}
# sscols[, CJ(edge1 = individual, edge2 = individual), by = group][edge1 < edge2]
#
#
# sscols
# setkey(sscols, CYCLER_ID, MOVEMENT)
# res <- sscols[CJ(CYCLER_ID, MOVEMENT, unique = TRUE)
#     ][, lapply(.SD, sum), by = .(CYCLER_ID, MOVEMENT)
#       ][is.na(odds), odds := 0]
