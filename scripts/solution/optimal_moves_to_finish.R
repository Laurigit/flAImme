

# #aggr_to_slots <- aggr_to_slots[1:30]
# cycler_id <- 6
# optimal_moves_to_finish(6, game_status, deck_status)
optimal_moves_to_finish <- function(cycler_deck_status, calc_from_slot, precalc_data, use_draw_odds = FALSE) {


trRes <- tryCatch({

    #cut track
    aggr_to_slots <-  precalc_data$aggr_to_slots[GAME_SLOT_ID >= calc_from_slot]





rivi_lkm <- aggr_to_slots[, .N]
ascend_v <- aggr_to_slots[, ascend_v]
#restricted_v <- aggr_to_slots[, restricted_v]
max_move_vect <-  aggr_to_slots[, MAXIMUM_MOVEMENT]
aggr_to_slots[, row_number := seq_len(.N)]
finish_slot <- aggr_to_slots[FINISH == 1, row_number ]
if (length(finish_slot) == 0 ) {
  finish_slot <- 0
}

if (finish_slot <= 3) {
  turns_to_finish_res <- data.table(TURNS_TO_FINISH = 1)
} else {



kortit_Dt <- data.table(MOVEMENT = as.numeric(str_split(cycler_deck_status[[1]][1], "")[[1]]))



kortit_aggr <- kortit_Dt[, .(cards_in_hand = .N), by = MOVEMENT]
kortit <- kortit_aggr[, MOVEMENT]
card_count <- kortit_aggr[, cards_in_hand]

#kortit_Dt <- deck_status_input

#append extra exh
extra_exh <- data.table(MOVEMENT = 2, cards_in_hand = 15)
appendaa <- rbind(kortit_aggr, extra_exh)
kortit <-  appendaa[, MOVEMENT]
card_count <- appendaa[, cards_in_hand]


if (use_draw_odds[[1]][1] != "") {

  odds_filtter <- use_draw_odds
 # odds_filtter <- draw_odds[1:8]# draw_odds[2:8, .(Turn_to_Draw = Turn_to_Draw + 1, MOVEMENT, prob = 0.5)]
  max_odds <- odds_filtter[, max(Turn_to_Draw)]
  compare_data <- CJ.dt(data.table(Turn_to_Draw = 1:max_odds), data.table(MOVEMENT = kortit))
  join_compare <- odds_filtter[compare_data, on = .(MOVEMENT, Turn_to_Draw)]
  missing_odds <- join_compare[is.na(prob)]
  missing_odds[, keep_me := FALSE]
  modds_data <- missing_odds[, .(Turn_to_Draw, MOVEMENT, keep_me)]
  #pad missing turns
  pad_data <- compare_data[!Turn_to_Draw %in% missing_odds[, unique(Turn_to_Draw)]][, keep_me := TRUE]
  append_pad <- rbind(pad_data, modds_data)

  } else {
    append_pad <- NULL
}




res <- MILPModel() %>%
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
  add_constraint(sum_expr(x[i, j, k], i = 1:rivi_lkm, j = 1:rivi_lkm) <= card_count[k], k = 1:length(kortit))  %>%


#add_constraint(ruudut[i, j] * x[i, j, k] <= y[k] * kortit[k], j = 1:rivi_lkm, i = 1:rivi_lkm , k = 1:20) %>%
  #tätä ei tarvi, koska se on jo boundeissa
  #model %>% add_constraint(colwise(ruudut[i, j] * x[i, j, k] <=  kortit[k], j = 1:rivi_lkm, i = 1:rivi_lkm , k = 1:length(kortit)) #%>%

  #käytä kortti vain kerran
  #res <- model %>% # add_constraint(sum_expr(x[i, j, k], i = 1:rivi_lkm, j = 1:rivi_lkm) <= 1, k = 1:length(kortit)) %>%
   # set_objective(sum_expr(x[i, j, k], i = 1:rivi_lkm, j = 1:rivi_lkm, k = 1:length(kortit)), "min") %>%
    set_objective(sum_expr(x[i, j, k], i = 1:rivi_lkm, j = 1:rivi_lkm, k = 1:length(kortit)), "min") %>%
    #maaliin on päästävä
    add_constraint(sum_expr(x[i, j, k], i = 1:rivi_lkm, k = 1:length(kortit), j = finish_slot:rivi_lkm) == 1) %>%
    #use only at the end the extra exhaust

    #makse sure to continue where I left last turn.
    add_constraint(sum_expr(x[n, j, k], j = 1:rivi_lkm, k = 1:length(kortit)) == sum_expr(x[i, n,  k], i = 1:rivi_lkm, k = 1:length(kortit)), n = 2:(finish_slot - 1)) %>%
  #add_constraint(sum_expr(x[i, j], i = length(kortit)) == 1, j = 1:n) %>%
  solve_model(with_ROI(solver = "symphony", verbosity = -2))
   # objective_value(res)
    dt_result <- data.table(get_solution(res, x[i, j, k]))[value > 0][order(i)]
  # #%>%
  # filter(value > 0) %>%
  # arrange(i)
  # #print(res)
  # dt_result <-data.table(res)

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

# dtRes <-data.table(res)
# kortti_id <- kortit_Dt[, .N, by = MOVEMENT]
#
# alkup_kortit <- kortti_id[, .(MOVEMENT, seq_len(.N))]
# join_kortit <- dtRes[alkup_kortit, on = .(k = V2)]
#
# aggr_to_slots[, row_no := seq_len(.N)]
#
# join_rata <- join_kortit[aggr_to_slots, on = .(i = row_no)]
# join_rata[, pelattu_kortti := na.locf(MOVEMENT, na.rm = FALSE)]
#
# sscols <- join_rata[, .(i, PIECE_ATTRIBUTE, pelattu_kortti, k, MOVEMENT)]
# sscols
#

