combinations_template <- function(case_count, cyclers, cycler_options) {

#copi_deck <- copy(STG_DECK)
#add exh for rouler
#new_row <- data.table(DECK_ID = 1, CARD_ID = 2, Count = NA)
#add_it <- rbind(copi_deck, new_row)
#cyc_dec <- add_it[STG_CYCLER, on = .(DECK_ID == CYCLER_TYPE_ID), allow.cartesian = TRUE]
#filter_in_game <- cyc_dec[CYCLER_ID %in% c(cyclers), .(MOVEMENT  = CARD_ID, CYCLER_ID)]
#filter_in_game <- cycler_options
  # sscols <- range[, .(TEAM_ID, CYCLER_ID, MOVEMENT)]
 # sscols <- filter_in_game[, .N, by = .(CYCLER_ID, MOVEMENT)][, N := NULL]
  sscols <- cycler_options
  # sscols[, TEAM_ID := 0]

  #sscols <- res_move[sscols_all, on = .(CYCLER_ID, MOVEMENT)][OPTION == TRUE | CYCLER_ID != moving_cycler]

  aggr <- sscols[, .(move_options = .N, options = list(MOVEMENT)), by = CYCLER_ID][order(CYCLER_ID)]
  #total number of cases
  poss_combs <- prod(aggr[, move_options])

  cyclers <- sscols[, .N, by = CYCLER_ID][order(CYCLER_ID)][, CYCLER_ID]
  #total number of rows
  tot_rows <- length(cyclers) * poss_combs
  sample_size <- case_count

  cyc_count <- length(cyclers)
  cases_vector <-  1:poss_combs

  cases_sample <- sample(cases_vector, min(sample_size, length(cases_vector)))
  #cases_sample <-  1:100
  #cyc_dt <- data.table(CYCLER_ID = cyclers)

  #aggr[, first_cycler_options := aggr[1, move_options]]
  aggr[, cumulative_options := cumprod(move_options) / move_options]
  #aggr[, prev_cum_options := shift(cumulative_options, 1)]
  #aggr[, prev_cum_options := ifelse(is.na(prev_cum_options), 1, prev_cum_options)]


  # aggr[, first_cycler_options := aggr[1, move_options]]
  #  aggr[, cumulative_options := cumprod(move_options) / first_cycler_options]
  #  aggr[, prev_cum_options := shift(cumulative_options, 1)]
  # aggr[, prev_cum_options := ifelse(is.na(prev_cum_options), 1, prev_cum_options)]
  dt_sample <- CJ.dt(aggr, data.table(case_id = cases_sample)[order(case_id)])
  dt_sample[, nth_move := (floor((case_id -1) / cumulative_options) %% move_options + 1)]

  dt_sample[, MOVEMENT := options[[1]][nth_move], by = .(CYCLER_ID, case_id)]
  filter_zero <- dt_sample[, .(CYCLER_ID, MOVEMENT, case_id)][order(case_id, CYCLER_ID)]
  return(filter_zero)
}


