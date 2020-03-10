
con <- connDB(con, dbname_input = "flaimme")
rm("ADM_OPTIMAL_MOVES")
required_data(c("STG_TRACK", "STG_TRACK_PIECE", "ADM_CYCLER_DECK", "ADM_OPTIMAL_MOVES"))

track <- 2
cyclers <- c(1, 2, 3, 4, 5, 6)
game_status <- start_game(cyclers,track, STG_TRACK_PIECE, STG_TRACK)
#game_status <- set_cycler_position(1, 64, 1,  game_status)
deck_status <- create_decks(cyclers, ADM_CYCLER_DECK)

smart_cyclers <- c(5, 6)

#everybody draws
for(loop in cyclers) {
  deck_status <- draw_cards(loop, deck_status)
  # print(list(deck_status[CYCLER_ID == loop & Zone == "Hand", CARD_ID]))

}


pre_track <- precalc_track(game_status)
ctM_res <- cyclers_turns_MOVEMEMENT_combs(con, ADM_OPTIMAL_MOVES, game_status, deck_status, pre_track)
ctM_data <- ctM_res$ctM_data

range <- calc_move_range(game_status, deck_status, ctM_data, STG_TEAM)
range_ss <- range[, .(MOVEMENT = max(MOVEMENT)), by = .(CYCLER_ID, curr_pos)]
range_ss <- range[c(1, 6, 11, 17, 23, 27), .(MOVEMENT, CYCLER_ID, curr_pos, TEAM_ID)]
zoom(game_status)


sscols <- range[splitted_odds > 0, .(TEAM_ID, CYCLER_ID, odds, MOVEMENT)]

aggr <- sscols[, .N, by = CYCLER_ID]
poss_combs <- prod(aggr[, N])
cyclers <- sscols[, .N, by = CYCLER_ID][, CYCLER_ID]
tot_data <- data.table(del_me = "")
for (cyc_loop in cyclers) {
  tot_data <- CJ.dt(tot_data, sscols[CYCLER_ID == cyc_loop, .(CYCLER_ID, odds,  MOVEMENT)])

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


#CALC DRAW ODDS HERE AS IT DEPENDS ON PLAYED CARD!
range[, DRAW_ODDS :=  ifelse(CYCLER_ID %in% smart_cyclers, (calculate_draw_distribution_by_turn(CYCLER_ID,
                                                                                                play_card(CYCLER_ID,
                                                                                                          card_id = NULL,
                                                                                                          deck_status,
                                                                                                          game_id = 0,
                                                                                                          turn_id = 0,
                                                                                                          con = FALSE,
                                                                                                          card_row_id = NULL,
                                                                                                          MOVEMENT_PLAYED = MOVEMENT),
                                                                                                4, db_res = TRUE)),
                             ""), by = .(CYCLER_ID, MOVEMENT)]

#join deck left
cycler_movement <- range[, .(CYCLER_ID, MOVEMENT, DECK_LEFT, DRAW_ODDS)]
join_deck_left <- cycler_movement[filter_zero, on = .(CYCLER_ID, MOVEMENT)]





#curr pos
currpos <- game_status[CYCLER_ID > 0, .(CYCLER_ID, GAME_SLOT_ID)]
#add_drow_odds here

#calculate draw odds for smart cyclers
#jionteam
currpos_team <- STG_CYCLER[currpos, on = "CYCLER_ID"][, .(CYCLER_ID, TEAM_ID, curr_pos = GAME_SLOT_ID)]
#join tosimul
join_info <- currpos_team[join_deck_left, on = "CYCLER_ID"]






#aggr_odds <- join_info[case_id == 1, .(case_odds = prod(odds), cyc_vec = list(phaseless_simulation(game_status, 5, 6, 3, STG_CYCLER, .SD, TRUE))), by = case_id, .SDcols = c("MOVEMENT", "CYCLER_ID", "curr_pos", "TEAM_ID")]
join_info[, case_odds := prod(odds), by = case_id]
join_info[case_id < 50, new_cyc_pos := phaseless_simulation(game_status, 5, 6, 3, STG_CYCLER, .SD, TRUE), by = case_id, .SDcols = c("MOVEMENT", "CYCLER_ID", "curr_pos", "TEAM_ID")]

#join track
track_ss <- pre_track$aggr_to_slots[, .(new_cyc_pos = GAME_SLOT_ID, TRACK_LEFT)]
join_track_left <- track_ss[join_info, on = .(new_cyc_pos)]
join_track_left[!is.na(TRACK_LEFT), TURNS_TO_FINISH := as.double(finish_turns_db(con, TRACK_LEFT, DECK_LEFT, pre_track, new_cyc_pos,
                                                               draw_odds_raw_data = DRAW_ODDS, save_to_DB = TRUE)),
                by = .(new_cyc_pos, TRACK_LEFT, DECK_LEFT, DRAW_ODDS)]
#add draw_odds

rm(ADM_OPTIMAL_MOVES)
required_data("ADM_OPTIMAL_MOVES")

sorted <- aggr_odds[order(-case_odds)]
sorted[, cumsum_odds := cumsum(case_odds)]
sorted[cumsum_odds < 0.2]
appendloop
nextcol <- tot_data[, 4:6, with = FALSE]
setnames(nextcol, colnames(nextcol), c("CYCLER_ID", "odds", "MOVEMENT"))
nextcol[, case_id := seq_len(.N)]

sscols[, CJ(edge1 = individual, edge2 = individual), by = group][edge1 < edge2]

library(data.table)
sscols
setkey(sscols, CYCLER_ID, MOVEMENT)
res <- sscols[CJ(CYCLER_ID, MOVEMENT, unique = TRUE)
    ][, lapply(.SD, sum), by = .(CYCLER_ID, MOVEMENT)
      ][is.na(odds), odds := 0]
