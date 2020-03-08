sscols <- range_joined_team[splitted_odds > 0, .(TEAM_ID, CYCLER_ID, odds, MOVEMENT)]

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


aggr_odds <- filter_zero[, .(case_odds = prod(odds)), by = case_id]
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
