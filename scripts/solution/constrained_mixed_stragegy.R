

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

constrained_mixed_strategy <- function(input_data) {
join_count1 <- input_data
modelli <- MILPModel() %>%
  #i = movement,
  #j = team
  #x = mixed strategy henk koht osa
  #s = mixed strategy level change part
  #x+s = mixed strategy
  add_variable(x[i, j], i = 2:9, j = 1:3, type = "continuous") %>%
  add_variable(y[i, j], i = 2:9, j = 1:3, type = "continuous") %>% #soft constraint penalty positive
  add_variable(z[i, j], i = 2:9, j = 1:3, type = "continuous") %>% #soft constraint penalty negative
  #s is level increase to reach total p = 1.
  add_variable(s[i, j, d], i = 2:9 ,j = 1:3, d = 1, type = "continuous") %>%
  #level incerase s is has always same value
  add_constraint(sum_expr(s[n, j, d], d = 1) == sum_expr(s[n + 1, j, d], d = 1), n = 2:8, j = 1:3) %>%
  # add_constraint(sum_expr(x[n, j, k], j = 1:rivi_lkm, k = 1:length(kortit)) ==
  #                  sum_expr(x[i, n,  k], i = 1:rivi_lkm, k = 1:length(kortit)), n = 2:(finish_slot - 1)) %>%

  set_bounds(z[i, j], i = 2:9, j = 1:3, lb = 0) %>%
  set_bounds(y[i, j], i = 2:9, j = 1:3, lb = 0) %>%
  set_bounds(x[i, j], i = 2:9, j = 1:3, lb = -1, ub = 1) %>%
  set_bounds(s[i, j, d], i = 2:9, j = 1:3, d = 1, lb = 0, ub = 1) %>%
  #total strategy must be 1
  add_constraint(sum_expr(x[i, j] + s[i, j, d], i = 2:9,  d = 1) == 1, j = 1:3) %>%
  #hard constraint to ensure that start is possible
  add_constraint(x[i, j] + s[i, j, d] <=  mix_strat_cap(i, j, join_count1), i = 2:9 , j = 1:3, d = 1)  %>%
  #total probabilitiy must be greater than 0
  add_constraint(x[i, j] + s[i, j, d] >=  0, i = 2:9 , j = 1:3, d = 1)  %>%
  #soft constraint fot meeting target. makes x as close as possible to uncapped prob
  add_constraint((x[i, j] - z[i, j] + y[i, j] + s[i, j, d]) ==  target(i, j, join_count1), i = 2:9 , j = 1:3, d = 1)  %>%
  #only level increase from s is allowed
  add_constraint((x[i, j]) <=  target(i, j, join_count1), i = 2:9 , j = 1:3)  %>%

  set_objective(sum_expr(z[i, j] + y[i, j], i = 2:9, j = 1:3)  , "min") %>%
  # set_objective(sum_expr(fun_payoff(i, j, orig), i = 1:2, j = 1:2), "min") %>%
  solve_model(with_ROI(solver = "symphony", verbosity = -1))


dt_model_x <- data.table(get_solution(modelli, x[i, j]))
dt_model_s <- data.table(get_solution(modelli, s[i, j, d]))
join_x_s <- dt_model_x[dt_model_s, on = .(i, j)]
total_res <- join_x_s[, .(TEAM_ID = j, MOVEMENT = i, strategy = value + i.value)]
return(total_res)
}

# constrained_mixed_strategy_simple <- function(input_data) {
#   #INPUTDATA = MOVEMENT, target_strategy, capped_strategy
#
#    moves <- sort(input_data[, .N, by = MOVEMENT][, MOVEMENT])
#    length_moves <- length(moves)
#    moves_without_last <- moves[1:(length_moves -1)]
#
#
#    moves_len <- nrow(input_data)
#     moves <- 1:moves_len
#    moves_without_last <- 1:(moves_len-1)
#
#   modelli <- MILPModel() %>%
#
#     #i = movement,
#     #x = mixed strategy henk koht osa
#     #s = mixed strategy level change part
#     #x+s = mixed strategy
#     add_variable(x[i], i = moves, type = "continuous") %>%
#     add_variable(y[i], i = moves, type = "continuous") %>% #soft constraint penalty positive
#     add_variable(z[i], i = moves, type = "continuous") %>% #soft constraint penalty negative
#     #s is level increase to reach total p = 1.
#     add_variable(s[i, d], i = moves , d = 1, type = "continuous") %>%
#     #level incerase s is has always same value
#     add_constraint(sum_expr(s[n, d], d = 1) == sum_expr(s[n + 1, d], d = 1), n = moves_without_last) %>%
#     # add_constraint(sum_expr(x[n, j, k], j = 1:rivi_lkm, k = 1:length(kortit)) ==
#     #                  sum_expr(x[i, n,  k], i = 1:rivi_lkm, k = 1:length(kortit)), n = 2:(finish_slot - 1)) %>%
#     set_bounds(z[i], i = moves, lb = 0) %>%
#     set_bounds(y[i], i = moves, lb = 0) %>%
#     set_bounds(x[i], i = moves, lb = -1, ub = 1) %>%
#     set_bounds(s[i, d], i = moves, d = 1, lb = 0, ub = 1) %>%
#     #total strategy must be 1
#     add_constraint(sum_expr(x[i] + s[i, d], i = moves,  d = 1) == 1) %>%
#     #hard constraint to ensure that start is possible
#     add_constraint(x[i] + s[i, d] <=  mix_strat_cap_simple(i, input_data), i = moves , d = 1)  %>%
#     #total probabilitiy must be greater than 0
#     add_constraint(x[i] + s[i, d] >=  0, i = moves , d = 1)  %>%
#     #soft constraint fot meeting target. makes x as close as possible to uncapped prob
#     add_constraint((x[i] - z[i] + y[i] + s[i, d]) ==  target_simple(i, input_data), i = moves, d = 1)  %>%
#     #only level increase from s is allowed
#     add_constraint((x[i]) <=  target_simple(i, input_data), i = moves )  %>%
#
#     set_objective(sum_expr(z[i] + y[i], i = moves)  , "min") %>%
#     # set_objective(sum_expr(fun_payoff(i, j, orig), i = 1:2, j = 1:2), "min") %>%
#     solve_model(with_ROI(solver = "symphony", verbosity = -2))
#
#
#   dt_model_x <- data.table(get_solution(modelli, x[i]))
#   dt_model_s <- data.table(get_solution(modelli, s[i, d]))
#   join_x_s <- dt_model_x[dt_model_s, on = .(i)]
#   total_res <- join_x_s[, .( MOVEMENT = i, strategy = value + i.value)]
#   return(total_res[, strategy])
# }
#
#
#
# mix_strat_cap_simple <- function(i, input_data) {
#  # ss_data <- input_data[, .(TEAM_ID, MOVEMENT, draw_odds)]
#   # dt_input <- data.table(i)
#   # joinaa <- input_data[dt_input, on = .(MOVEMENT == i)]
#   # joinaa[, fixed_res := ifelse(is.na(mixed_cap), 0, mixed_cap)]
#   res <- input_data[i, mixed_cap]
#   return(res)
#   #return(0.23)
# }
#
# target_simple <- function(i, input_data) {
#   #ss_data <- input_data[, .(TEAM_ID, MOVEMENT, new_prob_uncapped)]
#
#   res <- input_data[i, target_strat]
#   return(res)
# }
