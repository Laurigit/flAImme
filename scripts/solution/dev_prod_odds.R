
con <- connDB(con, dbname_input = "flaimme")
rm("ADM_OPTIMAL_MOVES")
required_data(c("STG_TRACK", "STG_TRACK_PIECE", "ADM_CYCLER_DECK", "ADM_OPTIMAL_MOVES"))

track <- 2

game_status <- start_game(c(1, 3, 5, 2, 4, 6),track, STG_TRACK_PIECE, STG_TRACK)
#game_status <- set_cycler_position(1, 64, 1,  game_status)
deck_status <- create_decks(c(1, 2, 3, 4, 5, 6), ADM_CYCLER_DECK)
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

#curr pos
currpos <- game_status[CYCLER_ID > 0, .(CYCLER_ID, GAME_SLOT_ID)]
#jionteam
currpos_team <- STG_CYCLER[currpos, on = "CYCLER_ID"][, .(CYCLER_ID, TEAM_ID, curr_pos = GAME_SLOT_ID)]
#join tosimul
join_info <- currpos_team[filter_zero, on = "CYCLER_ID"]

aggr_odds <- join_info[case_id == 1, .(case_odds = prod(odds), cyc_vec = list(phaseless_simulation(game_status, 5, 6, 3, STG_CYCLER, .SD, TRUE))), by = case_id, .SDcols = c("MOVEMENT", "CYCLER_ID", "curr_pos", "TEAM_ID")]
join_info[case_id < 15, cyc_vect := phaseless_simulation(game_status, 5, 6, 3, STG_CYCLER, .SD, TRUE), by = case_id, .SDcols = c("MOVEMENT", "CYCLER_ID", "curr_pos", "TEAM_ID")]
aggr_odds
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
