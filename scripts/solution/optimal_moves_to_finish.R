

# #aggr_to_slots <- aggr_to_slots[1:30]
# cycler_id <- 6
# optimal_moves_to_finish(6, game_status, deck_status)
optimal_moves_to_finish <- function(cycler_id, game_status, deck_status, move_cycler_amount = FALSE, use_draw_odds = FALSE, precalc_data,
                                    use_landing_slot = FALSE) {


trRes <- tryCatch({

temp_game_status_for_debugging <- game_status[1 != 0 , .(GAME_SLOT_ID,
                                                         PIECE_ATTRIBUTE,

                                                         FINISH,
                                                         EXTRA,
                                                         LANE_NO,
                                                         SQUARE_ID,
                                                         CYCLER_ID)]




    aggr_to_slots <- precalc_data$aggr_to_slots
    all_cycler_pos <- precalc_data$cycler_pos
    cycler_pos <- all_cycler_pos[CYCLER_ID == cycler_id]



deck_status_input <- deck_status[CYCLER_ID == cycler_id & Zone != "Removed"]

if (move_cycler_amount != FALSE) {
  if (use_landing_slot != FALSE) {
    new_cycler_pos <- set_cycler_position(cycler_id, use_landing_slot, 1, temp_game_status_for_debugging)

  } else {
  new_cycler_pos <- move_cycler(temp_game_status_for_debugging, cycler_id, move_cycler_amount, ignore_block = TRUE)

  }

  #cut the optimization track to start from the first move
  aggr_to_slots <- aggr_to_slots[GAME_SLOT_ID > new_cycler_pos[CYCLER_ID == cycler_id, max(GAME_SLOT_ID)]]
  deck_status_input <- play_card(cycler_id, move_cycler_amount, deck_status_input, 0, 0, FALSE)
}

rivi_lkm <- aggr_to_slots[, .N]
ascend_v <- aggr_to_slots[, ascend_v]
restricted_v <- aggr_to_slots[, restricted_v]
aggr_to_slots[, row_number := seq_len(.N)]
finish_slot <- aggr_to_slots[FINISH == 1, row_number ]




cyc_dec <- deck_status_input[CYCLER_ID == cycler_id & Zone != "Removed"]

kortit_Dt <-  rbind(data.table(CYCLER_ID = cycler_id, CARD_ID = 2, Zone = "Deck", MOVEMENT = 2, row_id = 1:10), cyc_dec)
kortit_aggr <- kortit_Dt[, .(cards_in_hand = .N), by = MOVEMENT]
kortit <- kortit_aggr[, MOVEMENT]
card_count <- kortit_aggr[, cards_in_hand]

if (use_draw_odds == TRUE) {
draw_odds <- calculate_draw_distribution_by_turn(cycler_id, deck_status_input, how_many_cards = 4)
} else {
  draw_odds <- NULL
}
# deck_status_input[CYCLER_ID == 1]
# draw_odds
#input_draw_odds_threshold <- 0.5
#filtered <- draw_odds[prob > input_draw_odds_threshold]
#input_draw_odds_threshold

#odds_filtter <-draw_odds
boundi_filtteri <- function(i, j, k, restricted_v, ascend_v, odds_filtter) {
#browser()
  rividata <- data.table(i, j, k, restricted_v, ascend_v)
  rividata[, distance := j - i]
  rividata[, tulos_save := !(distance >= 2 & k == distance)]

  rividata[ascend_v == TRUE & distance == 5, tulos_save := !(k >= 2 & k <= 5)] #caset, missä pääsee laskeen alamäkeen
  rividata[ascend_v == TRUE & distance < 5, tulos_save := TRUE] #mahdoton lasketella alle 5
  rividata[ascend_v == TRUE & distance > 5, tulos_save := !(k == distance)] #voi mennä kovempaa ku 5 alamäessä
  if (!is.null(odds_filtter)) {
  max_odds <- odds_filtter[, max(Turn_to_Draw)]
  joini <- odds_filtter[rividata, on = .(Turn_to_Draw  == i,  MOVEMENT == k)]
  joini[Turn_to_Draw <= max_odds & is.na(prob), tulos_save := TRUE]

  } else {
    joini <- rividata
  }
#  rividata[, tulos_save := !(((j - i) <= k)  & ((j - i) >= 2))] #either of these cases is true, then it is filterd out
  return(joini[, tulos_save])
}




model <- MILPModel() %>%
  #  add_variable(y[k], k = 1:20, type = "binary") %>%
  add_variable(x[i, j, k], i = 1:rivi_lkm, j = 1:rivi_lkm, k = 1:length(kortit), type = "binary") %>%
  #set_objective(sum_expr(y[k], k = 1), "max") %>%
  set_objective(sum_expr(x[i, j, k], i = 1:rivi_lkm, j = 1:rivi_lkm, k = 1:length(kortit)), "min") %>%
  set_bounds(x[i, j, k], i = 1:rivi_lkm, j = 1:rivi_lkm, k = 1:length(kortit), ub = 0, boundi_filtteri(i, j, kortit[k], restricted_v, ascend_v, draw_odds )) %>%
 add_constraint(sum_expr(x[i, j, k], i = 1:rivi_lkm, j = 1:rivi_lkm) <= card_count[k], k = 1:length(kortit)) # %>%

#add_constraint(ruudut[i, j] * x[i, j, k] <= y[k] * kortit[k], j = 1:rivi_lkm, i = 1:rivi_lkm , k = 1:20) %>%
  #tätä ei tarvi, koska se on jo boundeissa
  #model %>% add_constraint(colwise(ruudut[i, j] * x[i, j, k] <=  kortit[k], j = 1:rivi_lkm, i = 1:rivi_lkm , k = 1:length(kortit)) #%>%

  #käytä kortti vain kerran
  res <- model %>% # add_constraint(sum_expr(x[i, j, k], i = 1:rivi_lkm, j = 1:rivi_lkm) <= 1, k = 1:length(kortit)) %>%
  #eka sarake alotettava
   add_constraint(sum_expr(x[i, j, k], j = 1:rivi_lkm, k = 1:length(kortit)) == 1, i = 1:1) %>%

  #jatka siitä mihin jäit
  add_constraint(sum_expr(x[n, j, k], j = 1:rivi_lkm, k = 1:length(kortit)) == sum_expr(x[i, n,  k], i = 1:rivi_lkm, k = 1:length(kortit)), n = 2:(finish_slot - 1)) %>%
  # add_constraint(x[i, j, k]  <= sum_expr(x[i + kortit[k], j, k], i = 1:7, j = 1:length(kortit), k = 1:5) , i = 1:10, j = 1:10, k = 1:5) %>%# )
  #maaliin on päästävä
  add_constraint(sum_expr(x[i, j, k], i = 1:rivi_lkm, k = 1:length(kortit), j = finish_slot:rivi_lkm) == 1) %>%

  #add_constraint(sum_expr(x[i, j], i = length(kortit)) == 1, j = 1:n) %>%
  solve_model(with_ROI(solver = "symphony", verbosity = -2)) %>%
  get_solution(x[i, j, k]) %>%
  filter(value > 0) %>%
  arrange(i)


dtres <- cbind(data.table(res), cycler_id)
turns_to_finish_res <- dtres[, .(Turns_to_finish = .N), by = cycler_id]

return(turns_to_finish_res)

}, error = function(e) {
  return(data.table(cycler_id, Turns_to_finish = 0))
})
}
# alkup_kortit <- kortit_Dt[, .(MOVEMENT, seq_len(.N))]
# join_kortit <- dtres[alkup_kortit, on = .(k = V2)]
#
# join_rata <- join_kortit[aggr_to_slots, on = .(i = GAME_SLOT_ID)]
# join_rata[, pelattu_kortti := na.locf(MOVEMENT, na.rm = FALSE)]
#
# sscols <- join_rata[, .(i, PIECE_ATTRIBUTE, pelattu_kortti, k)]
# sscols
#

