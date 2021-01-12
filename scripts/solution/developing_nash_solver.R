
dataformaatti

#TEAM_ID, case_id, other_actions, my_action, pay_off
ss_orig <- orig[case_id < 5 & TEAM_ID < 3]
moves <- c(1, 1, 1, 2, 2, 1, 2, 2)
ss_orig[, MOVES := moves]
opp_movs <- c(1, 1, 2, 1, 1, 2, 2, 2)
ss_orig[, opponent_moves := opp_movs]

ii <- c(1, 1, 2, 2)
jj <- c(1, 2, 1, 2)

fun_payoff <- function(ii,  jj, orig) {

  inp_data <- data.table(ii, jj)
browser()
  payoff <- c(9, -2)

  # pf_matrix <- array(c(1, 0, 0, 1, 1, 0, 0, 1), c(2, 2, 2))
  #
  # inp_data[, payoff := ifelse(ii == jj, 1, 0)]

res <- payoff
  return(res)
}

modelli <- MILPModel() %>%
  #i = pelaaja,
  #j = action
  add_variable(x[i, j], i = 1:2, j = 1:2, type = "continuous") %>%
  add_constraint(sum_expr(x[i, j], j = 1:2) == 1, i = 1:2) %>%
 # add_constraint(sum_expr(fun_payoff(i, j, orig) * x[i, j] == fun_payoff(i, j + 1, orig) * x[i, j], i = 1:1, j = 1:1)) %>%
   add_constraint(sum_expr( x[i, j] * fun_payoff(i, j, orig), i = 1:2) == 1 , j = 1:1)  %>%
  set_objective(sum_expr( x[i, j], i = 1:1, j = 1:2), "min") %>%
  # set_objective(sum_expr(fun_payoff(i, j, orig), i = 1:2, j = 1:2), "min") %>%
              solve_model(with_ROI(solver = "symphony", verbosity = -2)) %>%
                # objective_value(res)
                get_solution(x[i, j]) %>%
              #  filter(value > 0) %>%
                arrange(i)
modelli

model <- MILPModel() %>%
  #  add_variable(y[k], k = 1:20, type = "binary") %>%
  add_variable(x[i, j, k], i = 1:rivi_lkm, j = 1:rivi_lkm, k = 1:length(kortit), type = "binary") %>%
  #set_objective(sum_expr(y[k], k = 1), "max") %>%

  set_bounds(x[i, j, k], i = 1:rivi_lkm, j = 1:rivi_lkm, k = 1:length(kortit), ub = 0, boundi_filtteri(i, j, kortit[k], max_move_vect, ascend_v, append_pad )) %>%
  #eka sarake alotettava
  add_constraint(sum_expr(x[i, j, k], j = 1:rivi_lkm, k = 1:length(kortit)) == 1, i = 1:1) %>%

  #jatka siitä mihin jäit

  # add_constraint(x[i, j, k]  <= sum_expr(x[i + kortit[k], j, k], i = 1:7, j = 1:length(kortit), k = 1:5) , i = 1:10, j = 1:10, k = 1:5) %>%# )
  #make sure that, normal cards are played before extra exhaust cards
  add_constraint(sum_expr(x[i, n, k], i = 1:rivi_lkm, k = length(kortit)) <=
                   sum_expr(x[n, j, k], j = 1:rivi_lkm, k = length(kortit)), n = 1:finish_slot) %>%
  #dont use more cards than you have
  add_constraint(sum_expr(x[i, j, k], i = 1:rivi_lkm, j = 1:rivi_lkm) <= card_count[k], k = 1:length(kortit)) # %>%


#add_constraint(ruudut[i, j] * x[i, j, k] <= y[k] * kortit[k], j = 1:rivi_lkm, i = 1:rivi_lkm , k = 1:20) %>%
#tätä ei tarvi, koska se on jo boundeissa
#model %>% add_constraint(colwise(ruudut[i, j] * x[i, j, k] <=  kortit[k], j = 1:rivi_lkm, i = 1:rivi_lkm , k = 1:length(kortit)) #%>%

#käytä kortti vain kerran
res <- model %>% # add_constraint(sum_expr(x[i, j, k], i = 1:rivi_lkm, j = 1:rivi_lkm) <= 1, k = 1:length(kortit)) %>%
  # set_objective(sum_expr(x[i, j, k], i = 1:rivi_lkm, j = 1:rivi_lkm, k = 1:length(kortit)), "min") %>%
  set_objective(sum_expr(x[i, j, k], i = 1:rivi_lkm, j = 1:rivi_lkm, k = 1:length(kortit)), "min") %>%
  #maaliin on päästävä
  add_constraint(sum_expr(x[i, j, k], i = 1:rivi_lkm, k = 1:length(kortit), j = finish_slot:rivi_lkm) == 1) %>%
  #use only at the end the extra exhaust

  #makse sure to continue where I left last turn.
  add_constraint(sum_expr(x[n, j, k], j = 1:rivi_lkm, k = 1:length(kortit)) == sum_expr(x[i, n,  k], i = 1:rivi_lkm, k = 1:length(kortit)), n = 2:(finish_slot - 1)) %>%
  #add_constraint(sum_expr(x[i, j], i = length(kortit)) == 1, j = 1:n) %>%
  solve_model(with_ROI(solver = "symphony", verbosity = -2)) %>%
  # objective_value(res)
  get_solution(x[i, j, k]) %>%
  filter(value > 0) %>%
  arrange(i)
#print(res)
dt_result <-data.table(res)

turns_to_finish_res <- dt_result[, .(TURNS_TO_FINISH = .N)]

if (turns_to_finish_res[, TURNS_TO_FINISH] > 22 | turns_to_finish_res[, TURNS_TO_FINISH] <= 0) {
  #not enough moves left

  turns_to_finish_res <- data.table(TURNS_TO_FINISH = 100)

}
}
return(turns_to_finish_res)

}, error = function(e) {

  warning("Optmization failed on trycatch")
  if (length(finish_slot) == 0 | finish_slot ==1 ) {
    res <- data.table(TURNS_TO_FINISH = 1)
  } else {

    if (turns_to_finish_res[, TURNS_TO_FINISH] > 22 | turns_to_finish_res[, TURNS_TO_FINISH] <= 0) {


      browser()
    }


  }
  # if(res[, TURNS_TO_FINISH] <= 0) {
  #   browser()
  # }
  return(res)
})
}
