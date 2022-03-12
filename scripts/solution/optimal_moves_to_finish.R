

# #aggr_to_slots <- aggr_to_slots[1:30]
# cycler_id <- 6
# optimal_moves_to_finish(6, game_status, deck_status)
optimal_moves_to_finish <- function(cycler_deck_status, calc_from_slot, precalc_data, draw_odds_raw_data = "") {






  track_left <- precalc_data[GAME_SLOT_ID == calc_from_slot, TRACK_LEFT]

  if (draw_odds_raw_data == "") {
    use_draw_odds <- ""
  } else {

    parse_draw_odds <- str_split(draw_odds_raw_data, ";")
    use_draw_odds <- data.table(Turn_to_Draw = as.numeric(str_split(parse_draw_odds[[1]][1], "")[[1]]),
                                  MOVEMENT =  as.numeric(str_split(parse_draw_odds[[1]][2], "")[[1]]),
                                  prob = -1)

  }




#trRes <- tryCatch({

    #cut track
  finish_slot_first <-  precalc_data[FINISH == 1, GAME_SLOT_ID ]
    aggr_to_slots <-  precalc_data[GAME_SLOT_ID >= calc_from_slot & GAME_SLOT_ID <= finish_slot_first ]






#restricted_v <- aggr_to_slots[, restricted_v]

aggr_to_slots[, row_number := seq_len(.N)]
finish_slot <- aggr_to_slots[FINISH == 1, row_number ]
max_move_vect <-  aggr_to_slots[1:finish_slot, MAXIMUM_MOVEMENT]
rivi_lkm <- aggr_to_slots[, .N]
ascend_v <- aggr_to_slots[1:finish_slot, ascend_v]
finsish_straight <- max(finish_slot - 9, 1)

kortit_Dt <- data.table(MOVEMENT = as.numeric(str_split(cycler_deck_status[[1]][1], "")[[1]]))

# if(nrow(kortit_Dt[MOVEMENT == 5]) > 0) {
#   penalty_card <- 5
# } else if (nrow(kortit_Dt[MOVEMENT == 4]) > 0) {
#   penalty_card <- 4
# } else if (nrow(kortit_Dt[MOVEMENT == 6]) > 0) {
#   penalty_card <- 6
# } else if (nrow(kortit_Dt[MOVEMENT == 3]) > 0) {
#   penalty_card <- 3
# } else if (nrow(kortit_Dt[MOVEMENT == 7]) > 0) {
#   penalty_card <- 7
# } else  {
#   penalty_card <- 2
# }


if (length(finish_slot) == 0 ) {
  finish_slot <- 0
}




# if (finish_slot <= 3) {
#   min_move <- aggr_to_slots[GAME_SLOT_ID == calc_from_slot, max(MINIMUM_MOVEMENT)]
#   max_move <- aggr_to_slots[GAME_SLOT_ID == calc_from_slot, max(MAXIMUM_MOVEMENT)]
#   last_move <- max(min(pmax(kortit_Dt[, max(MOVEMENT)], 2), max_move), min_move)
#   slot_over_is <- last_move - finish_slot + 1
#   turns_to_finish_res <- data.table(TURNS_TO_FINISH = 1, SLOTS_OVER_FINISH = slot_over_is, NEXT_MOVE = pmax(kortit_Dt[, max(MOVEMENT)], 2))
#
#
# } else {

if (nrow(kortit_Dt) == 0) {
  # slots_left_to_finish <- ceiling(finish_slot / 2)
  #
  # slots_over <- finish_slot %% 2
  # turns_to_finish_res <- data.table(TURNS_TO_FINISH = slots_left_to_finish, SLOTS_OVER_FINISH = slots_over, NEXT_MOVE = 2)
  kortit_Dt <- data.table(MOVEMENT = c(2,2,2,2,2,2,2,2))

}





kortit_aggr <- kortit_Dt[, .(cards_in_hand = .N), by = MOVEMENT]
max_result_of_turns_to_finish <- kortit_aggr[, sum(cards_in_hand)] + 1
kortit <- kortit_aggr[, MOVEMENT]
card_count <- kortit_aggr[, cards_in_hand]

#kortit_Dt <- deck_status_input

#append extra exh
# extra_exh <- data.table(MOVEMENT = 2, cards_in_hand = 15)
# appendaa <- rbind(kortit_aggr, extra_exh)
appendaa <- rbind(kortit_aggr)
kortit <-  appendaa[, MOVEMENT]
card_count <- appendaa[, cards_in_hand]


if (use_draw_odds[[1]][1] != "") {

  odds_filtter <- use_draw_odds
 # odds_filtter <- draw_odds[1:8]# draw_odds[2:8, .(Turn_to_Draw = Turn_to_Draw + 1, MOVEMENT, prob = 0.5)]
  max_odds <- odds_filtter[, max(Turn_to_Draw)]
  compare_data <- CJ.dt(data.table(Turn_to_Draw = 1:max_odds), data.table(MOVEMENT = kortit))
  join_compare <- odds_filtter[compare_data, on = .(MOVEMENT, Turn_to_Draw)]
  missing_odds <- join_compare#[is.na(prob)]
  missing_odds[, keep_me := ifelse(is.na(prob), FALSE, TRUE)]
  #modds_data <- missing_odds[, .(Turn_to_Draw, MOVEMENT, keep_me)]
  #pad missing turns
  #pad_data <- compare_data[!Turn_to_Draw %in% missing_odds[, unique(Turn_to_Draw)]][, keep_me := TRUE]
  #append_pad <- rbind(pad_data, modds_data)
  append_pad <- missing_odds[order(Turn_to_Draw, MOVEMENT)][Turn_to_Draw <= 2]
  } else {
    append_pad <- NULL
}


normal_model <- TRUE

res_mod <- MILPModel() %>%
  #  add_variable(y[k], k = 1:20, type = "binary") %>%
  add_variable(x[i, j, k], i = 1:rivi_lkm, j = 1:rivi_lkm, k = 1:length(kortit), type = "binary") %>%
  #add_variable(z[y], y = penalty_card, type = "integer") %>%
  #set_objective(sum_expr(y[k], k = 1), "max") %>%

  set_bounds(x[i, j, k], i = 1:rivi_lkm, j = 1:rivi_lkm, k = 1:length(kortit), ub = 0,
             boundi_filtteri(i, j, kortit[k], max_move_vect, ascend_v, append_pad )) %>%
  #eka sarake alotettava
  add_constraint(sum_expr(x[i, j, k], j = 1:rivi_lkm, k = 1:length(kortit)) == 1, i = 1) %>%

  #jatka siitä mihin jäit


  #make sure that, normal cards are played before extra exhaust cards
  # add_constraint(sum_expr(x[i, n, k], i = 1:rivi_lkm, k = length(kortit)) <=
  #                  sum_expr(x[n, j, k], j = 1:rivi_lkm, k = length(kortit)), n = 1:finish_slot) %>%
  #dont use more cards than you have
  add_constraint(sum_expr(x[i, j, k], i = 1:rivi_lkm, j = 1:rivi_lkm) <= card_count[k], k = 1:length(kortit))  %>%


    #use only at the end the extra exhaust

    #makse sure to continue where I left last turn.
    add_constraint(sum_expr(x[n, j, k], j = 1:rivi_lkm, k = 1:length(kortit)) ==
                     sum_expr(x[i, n,  k], i = 1:rivi_lkm, k = 1:length(kortit)), n = 2:(finish_slot - 1)) %>%
    #maaliin on päästävä
    add_constraint(sum_expr(x[i, j, k], i = 1:rivi_lkm, k = 1:length(kortit), j = finish_slot:rivi_lkm) == 1) %>%
  set_objective(sum_expr(x[i, j, k] * 1000, i = 1:rivi_lkm, j = 1:rivi_lkm, k = 1:length(kortit)) -

                  sum_expr(x[i, j, k] * colwise(last_card_payoff(i, j, k, kortit)), i = finsish_straight:(finish_slot - 1), j = finish_slot, k = 1:length(kortit))

                , "min") %>%

  solve_model(with_ROI(solver = "symphony", verbosity = -2, time_limit = 5))


if (res_mod$status != "success") {

  kortit_aggr <- kortit_Dt[, .(cards_in_hand = .N), by = MOVEMENT]
  add_row <- rbind(kortit_aggr, data.table(MOVEMENT = 100, cards_in_hand = 1))
  kortit <- add_row[, MOVEMENT]
  card_count <- add_row[, cards_in_hand]
  normal_model <- FALSE

  res_mod <-  MILPModel() %>%
    #  add_variable(y[k], k = 1:20, type = "binary") %>%
    add_variable(x[i, j, k], i = 1:rivi_lkm, j = 1:rivi_lkm, k = 1:length(kortit), type = "binary") %>%
    #add_variable(z[y], y = penalty_card, type = "integer") %>%
    #set_objective(sum_expr(y[k], k = 1), "max") %>%

    set_bounds(x[i, j, k], i = 1:rivi_lkm, j = 1:rivi_lkm, k = 1:length(kortit), ub = 0,
               boundi_filtteri(i, j, kortit[k], max_move_vect, ascend_v, append_pad )) %>%
    #eka sarake alotettava
    add_constraint(sum_expr(x[i, j, k], j = 1:rivi_lkm, k = 1:length(kortit)) == 1, i = 1) %>%

    #jatka siitä mihin jäit


    #make sure that, normal cards are played before extra exhaust cards
    # add_constraint(sum_expr(x[i, n, k], i = 1:rivi_lkm, k = length(kortit)) <=
    #                  sum_expr(x[n, j, k], j = 1:rivi_lkm, k = length(kortit)), n = 1:finish_slot) %>%
    #dont use more cards than you have
    add_constraint(sum_expr(x[i, j, k], i = 1:rivi_lkm, j = 1:rivi_lkm) <= card_count[k], k = 1:length(kortit))  %>%


    #makse sure to continue where I left last turn.
    add_constraint(sum_expr(x[n, j, k], j = 1:rivi_lkm, k = 1:length(kortit)) ==
                     sum_expr(x[i, n,  k], i = 1:rivi_lkm, k = 1:length(kortit)), n = 2:(finish_slot - 1))   %>%
    set_objective(sum_expr(x[i, j, k] * colwise(move_far_as_possible(i, j, k)), i = 1:(finish_slot - 1), j = finish_slot, k = length(kortit))
                  , "max") %>%

    solve_model(with_ROI(solver = "symphony", verbosity = -2))
  # turns_to_finish_res <- data.table(TURNS_TO_FINISH = max_result_of_turns_to_finish, SLOTS_OVER_FINISH = 0, NEXT_MOVE = pmax(kortit_Dt[, max(MOVEMENT)], 2))

}

res_mod$objective_value
raw <- as.data.table(res_mod$solution, keep.rownames = TRUE)[V2 > 0]
res <- nrow(raw)

raw[, c("i", "j", "k") := tstrsplit(V1, ",", fixed = TRUE)]
raw[, i_clean := as.numeric(gsub("\\D", "", i))]
raw[, j_clean := as.numeric(gsub("\\D", "", j))]
raw[, k_clean := as.numeric(gsub("\\D", "", k))]
setorder(raw, i_clean)

res_kortit <- data.table(MOVEMENT = kortit, k_clean = 1:length(kortit))

join_move <- res_kortit[raw, on = "k_clean"]
final_res <- join_move[, .(MOVEMENT, i_clean, SLOTS_PROGRESSED = j_clean - i_clean, j_clean)]

total_new_rows <- NULL
if (normal_model == FALSE) {

  start_slot <- final_res[MOVEMENT == 100, i_clean]
  joini_table <- aggr_to_slots[, .(ascend_v, i = seq_len(.N),  MOVEMENT_help_table = 100,  FINISH_LINE_HELP = finish_slot)]
 safety <- 1

  while ((start_slot < finish_slot) & safety < 10) {
    end_slot <-  start_slot + joini_table[i == start_slot, ascend_v]
    new_row <- data.table(MOVEMENT = 2, i_clean = start_slot, j_clean = end_slot)
    new_row[, SLOTS_PROGRESSED := j_clean - i_clean]
    total_new_rows <-  rbind(total_new_rows, new_row)
    start_slot <-  end_slot
    safety <- safety + 1

  }
 if (safety >= 10) { browser()}
  final_res <- rbind(final_res[MOVEMENT != 100], total_new_rows)
}


#parse all results as rows
total_data <- NULL
track_left_loop <- copy(track_left)
loop_dods <- copy(draw_odds_raw_data)
deck_left_loop <- copy(cycler_deck_status)
loop_res <- copy(final_res)
for (loop_row in 1:nrow(loop_res)) {
  last_starting_slot <- as.numeric(loop_res[nrow(loop_res), i_clean])
  last_starting_slot_info <- aggr_to_slots[last_starting_slot == row_number]
  last_movement_played <- as.numeric(loop_res[nrow(loop_res), MOVEMENT])
  last_move_actual <- max(min(last_starting_slot_info[, MAXIMUM_MOVEMENT], last_movement_played), last_starting_slot_info[, MINIMUM_MOVEMENT])
  next_move <- as.numeric(loop_res[1, MOVEMENT])
  slots_over_finish <- last_move_actual  +  last_starting_slot - finish_slot
  turns_to_finish <- nrow(loop_res)
  turns_to_finish_res <- data.table(TURNS_TO_FINISH = turns_to_finish, SLOTS_OVER_FINISH = slots_over_finish, NEXT_MOVE = next_move,
                                    TRACK_LEFT = track_left_loop, DRAW_ODDS = loop_dods, DECK_LEFT = deck_left_loop)
  total_data <-  rbind(total_data, turns_to_finish_res)
  SLOTS_PROGRESSED <- loop_res[1, SLOTS_PROGRESSED]
  track_left_loop <-  str_sub(track_left_loop, SLOTS_PROGRESSED + 1)

  deck_left_loop <- sub(next_move, "", deck_left_loop)
  loop_res <- loop_res[2:nrow(loop_res)]

  if (loop_dods != "") {
  parse_draw_odds <- str_split(loop_dods, ";")
  use_draw_odds <- data.table(Turn_to_Draw = as.numeric(str_split(parse_draw_odds[[1]][1], "")[[1]]),
                              MOVEMENT =  as.numeric(str_split(parse_draw_odds[[1]][2], "")[[1]]),
                              prob = -1)
  filter_dods <- use_draw_odds[Turn_to_Draw > 1]
  filter_dods[, Turn_to_Draw := Turn_to_Draw - 1]
  loop_dods <-  paste0(paste0(filter_dods[, Turn_to_Draw], collapse = ""), ";", paste0(filter_dods[, MOVEMENT], collapse = ""))
  if (loop_dods == ";") { loop_dods <- ""}
  }
}



#turns_to_finish_res <- data.table(TURNS_TO_FINISH = turns_to_finish, SLOTS_OVER_FINISH = slots_over_finish, NEXT_MOVE = next_move)



kesto <- difftime(Sys.time(), alotus, units = c("secs"))

return(total_data)


}


last_card_payoff <- function(i, j, k, kortit) {
dt <- data.table(i, j, k)
kortit_dt <- data.table(k = 1:length(kortit), kortit)
joinaa <- kortit_dt[dt, on = "k"]

return(joinaa[, kortit] + joinaa[, i] * 10)

}

move_far_as_possible <- function(i, j, k) {
  dt <- data.table(i, j, k)

  return(dt[, i])
}

