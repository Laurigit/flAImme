#test_calculate_mix_strategy_cap.R

#save(list = "MIXED_STRATEGY", file = "./test_data/calculate_mix_strategy.R")
load(file = "./test_data/calculate_mix_strategy.R")
testdata <- MIXED_STRATEGY


#calculate which cycler to move first
#calculate pick-order 1
#calculate prick order 2
#calculate combined mix_strat_cap


# <- function(data_for_calculations) {


#aggr_to_team <- join_track_left[, .(CYCLERS = paste0(CYCLER_ID, collapse = "_"),TEAM_SCORE = sum(TOT_SCORE), MOVES = paste0(MOVEMENT, collapse = "_")), by = .(TEAM_ID, case_id, opponent_moves, case_odds)]

#browser()
worse_move <- testdata#[TEAM_ID == 1]
# worse_move[, rank_ev := frank(-EV), by = TEAM_ID]
# worse_move[, rank_ev_orig := frank(-ORIG_EV), by = TEAM_ID]
# worse_move[TEAM_ID == 1]

# worse_move <- aggr_to_team[, .(
#   EV = mean(TEAM_SCORE)), by = .(CYCLERS, MOVES, TEAM_ID)][order(-EV)]

worse_move[, M1 := str_sub(MOVES, 1, 1)]
worse_move[, M2 := str_sub(MOVES, 3, 3)]
worse_move[, C1 := str_sub(CYCLERS, 1, 1)]
worse_move[, C2 := str_sub(CYCLERS, 3, 3)]
#aggr move1
# worse_move[, ':=' (,
#                              VAR = var(EV)), by = .(TEAM_ID, MOVEMENT = as.numeric(M1), CYCLER_ID = as.numeric(C1))]
# WVAR = sum(PROB_PRODUCT * (EV - WEIGH_EV) ^ 2)
move1_aggr <- worse_move[, .(WEIGH_EV = sum((EV * PROB_PRODUCT)) / sum(PROB_PRODUCT)), by = .(TEAM_ID, MOVEMENT = as.numeric(M1), CYCLER_ID = as.numeric(C1))]
weighted_variance <- move1_aggr[, .(CYCLER_ID = mean(CYCLER_ID), WVAR = var(WEIGH_EV)), by = .(TEAM_ID)]

move2_aggr <- worse_move[, .(WEIGH_EV = sum((EV * PROB_PRODUCT)) / sum(PROB_PRODUCT)), by = .(TEAM_ID, MOVEMENT = as.numeric(M2), CYCLER_ID = as.numeric(C2))]

weighted_variance2 <- move2_aggr[, .(CYCLER_ID = mean(CYCLER_ID), WVAR = var(WEIGH_EV)), by = .(TEAM_ID)]
total_ev_data <- rbind(move1_aggr, move2_aggr)
append_vars <- rbind(weighted_variance2, weighted_variance)
first_cycler_per_team <- append_vars[ , .SD[which.min(WVAR)], by = TEAM_ID]

first_cycler_ev <- total_ev_data[CYCLER_ID %in% first_cycler_per_team[, CYCLER_ID]]
gamma <- 0.9
first_cycler_ev[, top := exp(1 / gamma * WEIGH_EV) ]
first_cycler_ev[, bottom := sum(top), by = TEAM_ID]
first_cycler_ev[, new_prob_uncapped := top / bottom]
first_cycler_ev[, new_prob_provisional := top / bottom]
aggr_card_count <- deck_status[Zone != "Removed", .(count = .N), by = .(CYCLER_ID, MOVEMENT)]
join_count1 <- aggr_card_count[first_cycler_ev, on = .(MOVEMENT, CYCLER_ID)][order(TEAM_ID, - new_prob_uncapped)]
join_count1[, PRIO_GROUP := seq_len(.N), by = .(TEAM_ID)]
join_count1[, draw_odds := exact_draw_odds_outer_vectorized(draw_odds_data = .SD), by = TEAM_ID, .SDcols = c("PRIO_GROUP", "count")]
join_count1[, P_played_if_drawn := new_prob_uncapped / draw_odds]


}

mix_strat_cap <- function(i, j, input_data) {
  ss_data <- input_data[, .(TEAM_ID, MOVEMENT, draw_odds)]
  dt_input <- data.table(i, j)
  joinaa <- ss_data[dt_input, on = .(MOVEMENT == i, TEAM_ID == j)]
  joinaa[, fixed_res := ifelse(is.na(draw_odds), 0, draw_odds)]
 return(joinaa[, fixed_res])
  #return(0.23)
}

target <- function(i, j, input_data) {
  ss_data <- input_data[, .(TEAM_ID, MOVEMENT, new_prob_uncapped)]
  dt_input <- data.table(i, j)
  joinaa <- ss_data[dt_input, on = .(MOVEMENT == i, TEAM_ID == j)]
  joinaa[, fixed_res := ifelse(is.na(new_prob_uncapped), 0, new_prob_uncapped)]
  return(joinaa[, fixed_res])
}

modelli <- MILPModel() %>%
  #i = pelaaja,
  #j = action
  add_variable(x[i, j], i = 2:9, j = 1:3, type = "continuous") %>%
  add_variable(y[i, j], i = 2:9, j = 1:3, type = "continuous") %>% #soft constraint penalty positive
  add_variable(z[i, j], i = 2:9, j = 1:3, type = "continuous") %>% #soft constraint penalty negative
  add_variable(s[i, j, d], i = 2:9 ,j = 1:3, d = 1, type = "continuous") %>%
  add_constraint(sum_expr(s[n, j, d], d = 1) == sum_expr(s[n + 1, j, d], d = 1), n = 2:8, j = 1:3) %>%
  # add_constraint(sum_expr(x[n, j, k], j = 1:rivi_lkm, k = 1:length(kortit)) ==
  #                  sum_expr(x[i, n,  k], i = 1:rivi_lkm, k = 1:length(kortit)), n = 2:(finish_slot - 1)) %>%

  set_bounds(z[i, j], i = 2:9, j = 1:3, lb = 0) %>%
  set_bounds(y[i, j], i = 2:9, j = 1:3, lb = 0) %>%
  set_bounds(x[i, j], i = 2:9, j = 1:3, lb = -1, ub = 1) %>%
  set_bounds(s[i, j, d], i = 2:9, j = 1:3, d = 1, lb = 0, ub = 1) %>%
  add_constraint(sum_expr(x[i, j] + s[i, j, d], i = 2:9,  d = 1) == 1, j = 1:3) %>%
  add_constraint(x[i, j] + s[i, j, d] <=  mix_strat_cap(i, j, join_count1), i = 2:9 , j = 1:3, d = 1)  %>%
  #total probabilitiy must be greater than 0
  add_constraint(x[i, j] + s[i, j, d] >=  0, i = 2:9 , j = 1:3, d = 1)  %>%
  add_constraint((x[i, j] - z[i, j] + y[i, j] + s[i, j, d]) ==  target(i, j, join_count1), i = 2:9 , j = 1:3, d = 1)  %>%
  add_constraint((x[i, j]) <=  target(i, j, join_count1), i = 2:9 , j = 1:3)  %>%

  set_objective(sum_expr(z[i, j] + y[i, j], i = 2:9, j = 1:3)  , "min") %>%
  # set_objective(sum_expr(fun_payoff(i, j, orig), i = 1:2, j = 1:2), "min") %>%
  solve_model(with_ROI(solver = "symphony", verbosity = -1))

dt_model_x <- data.table(get_solution(modelli, x[i, j]))
dt_model_s <- data.table(get_solution(modelli, s[i, j, d]))
join_x_s <- dt_model_x[dt_model_s, on = .(i, j)]
total_res <- join_x_s[, .(TEAM_ID = j, MOVEMENT = i, strategy = value + i.value)]
join_to_input <- total_res[join_count1, on = .(TEAM_ID, MOVEMENT)]
join_to_input <- join_count1[total_res, on = .(TEAM_ID, MOVEMENT), .(MOVEMENT, TEAM_ID, new_prob_uncapped, draw_odds, strategy)]
join_to_input
join_to_input[, capped := strategy == draw_odds]

dt_model_x[, sum(value), by = j]



join_to_input[, sum(value), by = j]
append_long <- rbind(join_count1, join_count2)
#join first cyc
join_first_cyc <- first_cycler_per_team[append_long, on = .(TEAM_ID, CYCLER_ID),. (WVAR, MOVEMENT, CYCLER_ID, TEAM_ID, draw_odds_1 = draw_odds, PRIO_GROUP_1 = PRIO_GROUP)]

#make worse move to long format
worse_move1 <- worse_move[, .(MOVEMENT = as.numeric(M1), CYCLER_ID = as.numeric(C1), OTHER_MOVE = as.numeric(M2), OTHER_CYCLER = as.numeric(C2), TEAM_ID, EV)]
worse_move2 <- worse_move[, .(MOVEMENT = as.numeric(M2), CYCLER_ID = as.numeric(C2), OTHER_MOVE = as.numeric(M1),  OTHER_CYCLER = as.numeric(C1),TEAM_ID, EV)]
append_worse <- rbind(worse_move1, worse_move2)
join_first_cyc_long <- join_first_cyc[append_worse, on = .(TEAM_ID, CYCLER_ID, MOVEMENT)][!is.na(WVAR)]
#calc second prio grou
setorder(join_first_cyc_long, TEAM_ID, PRIO_GROUP_1, -EV)
join_first_cyc_long[, PRIO_GROUP_2 := seq_len(.N), by = .(TEAM_ID, PRIO_GROUP_1)]
#join card count again
rename_card_data <- aggr_card_count[, .(OTHER_CYCLER = CYCLER_ID, OTHER_MOVE = MOVEMENT, count_2nd = count)]
join_card_count <- rename_card_data[join_first_cyc_long, on = .(OTHER_CYCLER, OTHER_MOVE)]

join_card_count[TEAM_ID == 1]






#join_card_count[TEAM_ID == 2 & PRIO_GROUP_1 == 1, draw_odds_test := exact_draw_odds_outer_vectorized(.SD), by = .(TEAM_ID, PRIO_GROUP_1), .SDcols = c("PRIO_GROUP_2", "count_2nd")]
join_card_count[, draw_odds_2 := exact_draw_odds_outer_vectorized(.SD), by = .(TEAM_ID, PRIO_GROUP_1), .SDcols = c("PRIO_GROUP_2", "count_2nd")]
join_card_count[, PROPABILITY := draw_odds_1 * draw_odds_2]
join_card_count[, prob_case_id := seq_len(.N)]
back_to_long_1 <- join_card_count[, .(CYCLER_ID, MOVEMENT, prob_case_id, PROPABILITY, TEAM_ID)]
back_to_long_2 <- join_card_count[, .(CYCLER_ID = OTHER_CYCLER, MOVEMENT = OTHER_MOVE, prob_case_id, PROPABILITY, TEAM_ID)]
result_long <- rbind(back_to_long_1, back_to_long_2)
setorder(result_long, TEAM_ID, prob_case_id, CYCLER_ID)

# join_back_to_track_left <- result_long[join_track_left, on = .(CYCLER_ID, TEAM_ID, MOVEMENT)]

aggregate_result <- result_long[, .(CYCLERS = paste0(CYCLER_ID, collapse = "_"), MOVES = paste0(MOVEMENT, collapse = "_")),
                                by = .(prob_case_id, TEAM_ID, PROPABILITY)][, prob_case_id := NULL][order(TEAM_ID, -PROPABILITY)]
#ready to join!
join_prob_to_cases <- aggregate_result[testdata, on = .(TEAM_ID, CYCLERS, MOVES)]
join_prob_to_cases[, helper_odds_for_other_odds_calc := ifelse(PROPABILITY == 0, 0.000000001, PROPABILITY)]
join_prob_to_cases[, ':=' (OTHER_ACTION_ODDS = (prod(helper_odds_for_other_odds_calc) / helper_odds_for_other_odds_calc),
                           PROB_PRODUCT = prod(PROPABILITY)), by = case_id]

sscols_case <- join_prob_to_cases[, .(PROB_PRODUCT, OTHER_ACTION_ODDS, case_id, TEAM_ID)]

join_prob_to_cases[, sum(PROPABILITY), by = .(opponent_moves, TEAM_ID)]
join_prob_to_cases[TEAM_ID == 1 & MOVES == "3_4" & PROPABILITY > 0]
#finally join case prob

# data_for_calculations[, ':=' (PROB_PRODUCT = NULL, OTHER_ACTION_ODDS = NULL)]
browser()
case_prob <- sscols_case[data_for_calculations, on = .(case_id, TEAM_ID)]
return(case_prob)
}
