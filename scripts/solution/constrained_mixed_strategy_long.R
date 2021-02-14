
mix_strat_cap_long <- function(i, j, k, input_data) {
  ss_data <- input_data[, .(TEAM_ID, MOVEMENT, OTHER_MOVE, mixed_cap)]
  dt_input <- data.table(i, j, k)
  joinaa <- ss_data[dt_input, on = .(MOVEMENT == i, TEAM_ID == j, OTHER_MOVE == k)]
  joinaa[, fixed_res := ifelse(is.na(mixed_cap), 0, mixed_cap)]
#  browser()
  return(joinaa[, fixed_res])
  #return(0.23)
}

target_long <- function(i, j, k, input_data) {
  ss_data <- input_data[, .(TEAM_ID, MOVEMENT, OTHER_MOVE, target_strat)]
  dt_input <- data.table(i, j, k)
  joinaa <- ss_data[dt_input, on = .(MOVEMENT == i, TEAM_ID == j, OTHER_MOVE == k)]
  joinaa[, fixed_res := ifelse(is.na(target_strat), 0, target_strat)]
 # browser()
  return(joinaa[, fixed_res])
}

constrained_mixed_strategy_long <- function(input_data) {
 # input_data <- join_card_count_to_second[TEAM_ID == 1 & OTHER_MOVE == 7]

  teams <- sort(input_data[, .N, by = TEAM_ID][, TEAM_ID])
  modelli <- MILPModel() %>%
    #i = movement,
    #j = team
    #x = mixed strategy henk koht osa
    #s = mixed strategy level change part
    #x+s = mixed strategy
    add_variable(x[i, j, k], i = 2:9, j = teams, k = 2:9, type = "continuous") %>%
    add_variable(y[i, j, k], i = 2:9, j = teams, k = 2:9, type = "continuous") %>% #soft constraint penalty positive
    add_variable(z[i, j, k], i = 2:9, j = teams, k = 2:9, type = "continuous") %>% #soft constraint penalty negative
    add_variable(q[i, j, k], i = 2:9, j = teams, k = 2:9, type = "binary") %>% #THIS FOR CASES WHERE WE DON*T HAVE DATA. K=8 and MOVEMENT = 8.
    #s is level increase to reach total p = 1.
    add_variable(s[i, j, k, d], i = 2:9 ,j = teams, k = 2:9, d = 1, type = "continuous") %>%
    #level incerase s is has always same value
    add_constraint(sum_expr(s[n, j, k, d], d = 1) == sum_expr(s[n + 1, j, k, d], d = 1), n = 2:8, j = teams, k = 2:9) %>%
    # add_constraint(sum_expr(x[n, j, k], j = 1:rivi_lkm, k = 1:length(kortit)) ==
    #                  sum_expr(x[i, n,  k], i = 1:rivi_lkm, k = 1:length(kortit)), n = 2:(finish_slot - 1)) %>%

    set_bounds(z[i, j, k], i = 2:9, k = 2:9, j = teams, lb = 0) %>%
    set_bounds(y[i, j, k], i = 2:9, k = 2:9, j = teams, lb = 0) %>%
    set_bounds(x[i, j, k], i = 2:9, k = 2:9, j = teams, lb = -1, ub = 1) %>%
    set_bounds(s[i, j, k, d], i = 2:9, k = 2:9, j = teams, d = 1, lb = 0, ub = 1) %>%
    #total strategy must be 1. q is here to pass the constraint when we have no data
    add_constraint(sum_expr(x[i, j, k] + s[i, j, k, d] + q[i, j, k], i = 2:9,  d = 1) == 1, j = teams, k = 2:9) %>%
    #hard constraint to ensure that start is possible
    add_constraint(x[i, j, k] + s[i, j, k, d] <=  mix_strat_cap_long(i, j, k, input_data), i = 2:9 , j = teams, k = 2:9, d = 1)  %>%
    #total probabilitiy must be greater than 0
    add_constraint(x[i, j, k] + s[i, j, k, d] >=  0, i = 2:9 , j = teams, k = 2:9, d = 1)  %>%
    #soft constraint fot meeting target. makes x as close as possible to uncapped prob
    add_constraint((x[i, j, k] - z[i, j, k] + y[i, j, k] + s[i, j, k, d]) ==  target_long(i, j, k, input_data), i = 2:9 , j = teams, k = 2:9, d = 1)  %>%
    #only level increase from s is allowed
    add_constraint((x[i, j, k]) <=  target_long(i, j, k, input_data), i = 2:9 , j = teams, k = 2:9)  %>%

    set_objective(sum_expr(z[i, j, k] + y[i, j, k] + q[i, j, k] * 100000, i = 2:9, j = teams, k = 2:9)  , "min") %>%
    # set_objective(sum_expr(fun_payoff(i, j, orig), i = 1:2, j = 1:2), "min") %>%
    solve_model(with_ROI(solver = "symphony", verbosity = -2))


  dt_model_x <- data.table(get_solution(modelli, x[i, j, k]))
  dt_model_s <- data.table(get_solution(modelli, s[i, j, k ,d]))
  #dt_model_q <- data.table(get_solution(modelli, q[i, j, k]))
  join_x_s <- dt_model_x[dt_model_s, on = .(i, j, k)]
  total_res <- join_x_s[, .(TEAM_ID = j, MOVEMENT = i, OTHER_MOVE = k, strategy = value + i.value)]
  #total_res[strategy > 0]
  return( total_res[strategy > 0])
}
