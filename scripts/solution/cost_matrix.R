

#aggr_to_slots <- aggr_to_slots[1:30]
cycler_id <- 1
optaimal_moves_to_finish <- function(cycler_id, game_status, deck_status)

temp_game_status_for_debugging <- game_status[1 != 0 , .(GAME_SLOT_ID,
                                                         PIECE_ATTRIBUTE,
                                                         START,
                                                         FINISH,
                                                         EXTRA,
                                                         LANE_NO,
                                                         SQUARE_ID,
                                                         CYCLER_ID)]

#temp_game_status_for_debugging <- set_cycler_position(cycler_id, 10, 1, temp_game_status_for_debugging)

cycler_pos <- temp_game_status_for_debugging[CYCLER_ID == cycler_id, GAME_SLOT_ID]


aggr_to_slots <- temp_game_status_for_debugging[is.na(EXTRA) &
                                                # GAME_SLOT_ID <= game_status[FINISH == 1, GAME_SLOT_ID] #&
                                                  GAME_SLOT_ID >= cycler_pos
                                                , .N, by = .(GAME_SLOT_ID, PIECE_ATTRIBUTE, FINISH)]


aggr_to_slots[, mountain_row := ifelse(PIECE_ATTRIBUTE == "M", 1, 0)]
aggr_to_slots[, start_of_restricted_movement := ifelse(shift(mountain_row == 1, n = 1, type = "lead") |
                                                         shift(mountain_row == 1, n = 2, type = "lead") |
                                                         shift(mountain_row == 1, n = 3, type = "lead") |
                                                         shift(mountain_row == 1, n = 4, type = "lead") |
                                                         shift(mountain_row == 1, n = 5, type = "lead") |
                                                         shift(mountain_row == 1, n = 6, type = "lead") | mountain_row == 1, 1, 0)]
aggr_to_slots[, start_of_restricted_movement := ifelse(is.na(start_of_restricted_movement), 0, start_of_restricted_movement)]
aggr_to_slots[, key := "1"]

restricted_v <- aggr_to_slots[, start_of_restricted_movement] == 1
ascend_v <- aggr_to_slots[, PIECE_ATTRIBUTE == "A"]

#function cost from slot A to B

landing_slots <- aggr_to_slots[, .(landing_slot = GAME_SLOT_ID, key = "1")]

#cartesian join
carti <- merge(x = landing_slots, y = aggr_to_slots, by = "key", all = TRUE, allow.cartesian = TRUE)
carti[, distance := landing_slot - GAME_SLOT_ID]
filter_nine_or_less <- carti #[distance <= 9 & distance >= 2]
filter_nine_or_less[, cost := cost_AB(GAME_SLOT_ID, landing_slot, PIECE_ATTRIBUTE, start_of_restricted_movement), by = .(landing_slot, GAME_SLOT_ID)]
filter_nine_or_less[distance > 9 | distance < 2, cost := 10000000]
filter_nine_or_less[, legal_move := ifelse(cost > 9, 0, 1)]
filter_nine_or_less[is.na(cost)]
matriisi <- as.matrix(filter_nine_or_less[, .(GAME_SLOT_ID, landing_slot)], rownames = "GAME_SLOT_ID")
rivi_lkm <- filter_nine_or_less[, uniqueN(GAME_SLOT_ID)]
sarake_lkm <- filter_nine_or_less[, uniqueN(landing_slot)]

cost_matrix <- matrix(data = filter_nine_or_less[, cost], nrow = rivi_lkm, ncol = sarake_lkm, byrow = FALSE)



cyc_dec <- deck_status[CYCLER_ID == cycler_id]

kortit_Dt <-  rbind(data.table(CYCLER_ID = input_cycler_id, CARD_ID = 2, Zone = "Deck", MOVEMENT = 2, row_id = 1:10), cyc_dec)
kortit_aggr <- kortit_Dt[, .(cards_in_hand = .N), by = MOVEMENT]
kortit <- kortit_aggr[, MOVEMENT]
card_count <- kortit_aggr[, cards_in_hand]

boundi_filtteri <- function(i, j, k, restricted_v, ascend_v) {
  rividata <- data.table(i, j, k, restricted_v, ascend_v)
  rividata[, distance := j - i]
  rividata[, tulos_save := !(distance >= 2 & k == distance)]

  rividata[ascend_v == TRUE & distance == 5, tulos_save := !(k >= 2 & k <= 5)] #caset, missä pääsee laskeen alamäkeen
  rividata[ascend_v == TRUE & distance < 5, tulos_save := TRUE] #mahdoton lasketella alle 5
  rividata[ascend_v == TRUE & distance > 5, tulos_save := !(k == distance)] #voi mennä kovempaa ku 5 alamäessä

#  rividata[, tulos_save := !(((j - i) <= k)  & ((j - i) >= 2))] #either of these cases is true, then it is filterd out
  return(rividata[, tulos_save])
}


model <- MILPModel() %>%
  #  add_variable(y[k], k = 1:20, type = "binary") %>%
  add_variable(x[i, j, k], i = 1:rivi_lkm, j = 1:rivi_lkm, k = 1:length(kortit), type = "binary") %>%
  #set_objective(sum_expr(y[k], k = 1), "max") %>%
  set_objective(sum_expr(x[i, j, k], i = 1:rivi_lkm, j = 1:rivi_lkm, k = 1:length(kortit)), "min") %>%
  set_bounds(x[i, j, k], i = 1:rivi_lkm, j = 1:rivi_lkm, k = 1:length(kortit), ub = 0, boundi_filtteri(i, j, kortit[k], restricted_v, ascend_v)) %>%
 add_constraint(sum_expr(x[i, j, k], i = 1:rivi_lkm, j = 1:rivi_lkm) <= card_count[k], k = 1:length(kortit)) # %>%
  #add_constraint(ruudut[i, j] * x[i, j, k] <= y[k] * kortit[k], j = 1:rivi_lkm, i = 1:rivi_lkm , k = 1:20) %>%
  #tätä ei tarvi, koska se on jo boundeissa
  #model %>% add_constraint(colwise(ruudut[i, j] * x[i, j, k] <=  kortit[k], j = 1:rivi_lkm, i = 1:rivi_lkm , k = 1:length(kortit)) #%>%

  #käytä kortti vain kerran
  res <- model %>% # add_constraint(sum_expr(x[i, j, k], i = 1:rivi_lkm, j = 1:rivi_lkm) <= 1, k = 1:length(kortit)) %>%
  #eka sarake alotettava
   add_constraint(sum_expr(x[i, j, k], j = 1:rivi_lkm, k = 1:length(kortit)) == 1, i = 1:1) %>%
  #ruudussa voi käydä vaan kerran
  add_constraint(sum_expr(x[i, j, k], j = 1:rivi_lkm, k = 1:length(kortit)) <= 1, i = 1:rivi_lkm)  %>%
  #jatka siitä mihin jäit
  add_constraint(sum_expr(x[n, j, k], j = 1:rivi_lkm, k = 1:length(kortit)) == sum_expr(x[i, n,  k], i = 1:rivi_lkm, k = 1:length(kortit)), n = 2:(rivi_lkm - 1)) %>%
  # add_constraint(x[i, j, k]  <= sum_expr(x[i + kortit[k], j, k], i = 1:7, j = 1:length(kortit), k = 1:5) , i = 1:10, j = 1:10, k = 1:5) %>%# )
  #maaliin on päästävä
  add_constraint(sum_expr(x[i, j, k], i = 1:rivi_lkm, k = 1:length(kortit)) == 1, j = rivi_lkm) %>%

  #add_constraint(sum_expr(x[i, j], i = length(kortit)) == 1, j = 1:n) %>%
  solve_model(with_ROI(solver = "symphony", verbosity = 1)) %>%
  get_solution(x[i, j, k]) %>%
  filter(value > 0) %>%
  arrange(i)


dtres <- data.table(res)
alkup_kortit <- kortit_Dt[, .(MOVEMENT, seq_len(.N))]
join_kortit <- dtres[alkup_kortit, on = .(k = V2)]

join_rata <- join_kortit[aggr_to_slots, on = .(i = GAME_SLOT_ID)]
join_rata[, pelattu_kortti := na.locf(MOVEMENT, na.rm = FALSE)]

sscols <- join_rata[, .(i, PIECE_ATTRIBUTE, pelattu_kortti, k)]
sscols




model <- MILPModel() %>%
  add_variable(x[i, j], i = 2:rivi_lkm, j = 2:rivi_lkm, type = "binary") %>%
  set_objective(sum_expr(cost_matrix[i, j] * x[i, j], i = 2:rivi_lkm, j = 2:rivi_lkm), "min")  %>%
  #add_constraint(sum_expr(weights[i] * x[i], i = 1:n) <= max_capacity) %>%
  set_bounds(x[i, j], ub = 0, i = 2:rivi_lkm, j = 2:rivi_lkm, is_legal(filter_nine_or_less, i, j)) %>%
  #need to start from slot 1
  add_constraint(sum_expr(x[i, j], j = 2:rivi_lkm) == 1, i = 2)%>%
  #need to finish
add_constraint(sum_expr(x[i, j], i = 2:rivi_lkm) == 1, j = rivi_lkm) %>%
#need to leave from each slot where entered
  add_constraint(x[i, j] == x[i + cost_matrix[i, j], j])
#add_constraint(sum_expr(x[i, j], i = 2:rivi_lkm) >= sum_expr(x[i, j], j = 2:(rivi_lkm-3)))

#add_constraint(x[i, j] == 0, i = 1:rivi_lkm, j = 1:rivi_lkm, is_legal(filter_nine_or_less, i, j))


  res <- solve_model(model, with_ROI(solver = "symphony")) %>%
  get_solution(x[i, j]) %>%
    filter(value > 0) #%>%

  res$solution
  $solution
  get_solution(x[i]) %>%
  filter(value > 0)



#radan pituus
pituus <- 100
#korttien määrä
kortit <- 15

#etäisyys matriisi. Radan koko on 40


g




model <- MIPModel() %>%
  add_variable(x[i, j], type = "binary", i = 1:pituus, j = 1:kortit)%>%
  set_objective(sum_expr(sum_expr(x[i, j],  i = 1:pituus == 0), j = 1:kortit), "min") %>%
  add_constraint((x[i, j] + x[i, j + 1]) <= 1, i = 1:(pituus), j = 1:(kortit - 1)) %>%
  add_constraint(sum_expr(x[i, j], i = 1:pituus <= 10, j = 1:kortit)) %>%
  add_constraint(sum_expr(x[i, j], j = 1:kortit == 1, i = 1:pituus))


result <- solve_model(model, with_ROI("symphony"))
#add_constraint((x[i, j] + x[i + 1 , j - 1]) == 1, i = 1:pituus, j = 1:kortit)
#set_objective(sum_expr(sum_expr(x[i, j],  i = 1:pituus) > 0, j = 1:kortit), direction = "min")


%>%
