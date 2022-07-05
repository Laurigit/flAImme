calc_combinations_data <- function(con, game_status, turn_start_deck_status, deck_status, pre_aggr_game_status_no_list, matr_ijk,
                                   reverse_slots_squares, slip_map_matrix,
                                    STG_CYCLER, calc_ttf = 0, case_count = 1000,
                                   hidden_info_teams = NULL, input_turn_id = NULL, finished_cyclers = as.numeric()
                                  ) {
#calc ttf takes in current turn of game

#  case_count  <- input_case_count
# pre_aggr_game_status_no_list <- pre_agg_no_list
#  input_turn_id <- turn_id
#    hidden_info_teams <- 4

drawn_deck_copied <- copy(deck_status)
deck_copied <- copy(turn_start_deck_status)

  cyclers <- game_status[CYCLER_ID > 0, CYCLER_ID]


 # sscols <- range[, .(TEAM_ID, CYCLER_ID, MOVEMENT)]
  sscols <- deck_copied[Zone != "Removed", .N, by = .(CYCLER_ID, MOVEMENT)][CYCLER_ID %in% cyclers][, N := NULL]

  # combinations_based_on_deck <- input_combinations_template[sscols, on = .(CYCLER_ID, MOVEMENT)]
  # full_cases <- combinations_based_on_deck[, .N, by = case_id][N == length(cyclers)][, case_id]
  # #tot_cases <- combinations_based_on_deck[, .N, by = case_id][, case_id]
  # sample_cases <- sample(full_cases, min(case_count), length(full_cases))
  # smaller_subset <- combinations_based_on_deck[case_id %in% sample_cases]
  #
  # filter_zero <- smaller_subset[, .(CYCLER_ID, MOVEMENT, case_id)][order(case_id, CYCLER_ID)]
  filter_zero <- combinations_template(case_count, cyclers, sscols)
  #curr pos
  currpos <- game_status[CYCLER_ID > 0, .(CYCLER_ID, curr_pos = GAME_SLOT_ID, curr_square = SQUARE_ID)]
  #add_drow_odds here

  join_curr_pos <- currpos[filter_zero, on = "CYCLER_ID"]


  #thinking_time <- 2
  setorder(join_curr_pos, case_id, -curr_square, -curr_pos)



  join_curr_pos[, c("cyc_id_copy",
                "MOVE_DIFF",
                "NEW_GAME_SLOT_ID",
                "EXHAUST",
                "NEW_SQUARE") := as.data.table(move_cycler_c(as.matrix(.SD), matr_ijk, reverse_slots_squares, slip_map_matrix)),
            by = .(case_id), .SDcols = c("CYCLER_ID", "MOVEMENT", "curr_pos")]



  new_positions_by_cycler <- join_curr_pos[, .N, by = .(NEW_GAME_SLOT_ID, MOVEMENT, CYCLER_ID)]
  new_positions_by_cycler[, row_id_calc := seq_len(.N)]

  new_positions_by_cycler[, DECK_LEFT := convert_deck_left_to_text(deck_copied,
                                                           CYCLER_ID, MOVEMENT, trunc = FALSE, max_extra_exhaust = 1),
                  by = .(row_id_calc)]

  #join team_id
  pos_with_team <- STG_CYCLER[, .(CYCLER_ID, TEAM_ID)][new_positions_by_cycler, on = "CYCLER_ID"]


copy_pre <- copy(pre_aggr_game_status_no_list)
ss_track_left <- copy_pre[, .(NEW_GAME_SLOT_ID = GAME_SLOT_ID, TRACK_LEFT)]
join_track_left <- ss_track_left[pos_with_team, on = .(NEW_GAME_SLOT_ID)]
join_track_left[, DRAW_ODDS := ""]
join_known <- ADM_OPTIMAL_MOVES_AGGR[join_track_left, on = .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS)]
#hidden_info_teams <- 1

if (!is.null(hidden_info_teams)) {

  #mission is to solve alternative TTF based on draw odds. first filter teams that are included in the calculation
#it's ok to use info which cards is drawn as long as it's not used to decide which cycler is moved first
  hidden_info_cyclers <- STG_CYCLER[TEAM_ID %in% hidden_info_teams, CYCLER_ID]
 hidden_team_options <- drawn_deck_copied[CYCLER_ID %in% hidden_info_cyclers & Zone == "Hand", .N, by = .(CYCLER_ID, MOVEMENT)][, N := NULL]
dod_teams <- join_track_left[hidden_team_options, on = .(CYCLER_ID, MOVEMENT)]
if (nrow(dod_teams) > 0) {
#calculate draw odds


suppressWarnings(dod_teams[, DRAW_ODDS := calculate_draw_distribution_by_turn(cycler_id = CYCLER_ID,
                                                                              current_decks_input = play_card(CYCLER_ID,card_id = NULL,
                                                                                             drawn_deck_copied,
                                                                                             game_id = 0,
                                                                                             turn_id = 0,
                                                                                             con = NULL,
                                                                                             card_row_id = FALSE,
                                                                                             MOVEMENT_PLAYED = MOVEMENT,
                                                                                             force = TRUE,
                                                                                             copy = TRUE),
                                                                              how_many_cards = 4,
                                                                              db_res = TRUE),
          by = .(DECK_LEFT, TRACK_LEFT, NEW_GAME_SLOT_ID, MOVEMENT, CYCLER_ID)])



#some cases have all options available, so no draw odds
dod_found <- dod_teams[DRAW_ODDS != ""]

#now I know draw odds. Check if I have already solved these
if (nrow(dod_found) > 0) {

joined_known_dods <- ADM_OPTIMAL_MOVES_AGGR[dod_found, on = .(DECK_LEFT, TRACK_LEFT, DRAW_ODDS)][, .(DECK_LEFT, TRACK_LEFT, DRAW_ODDS, MOVEMENT,
                                                                                                     NEW_GAME_SLOT_ID, TURNS_TO_FINISH, SLOTS_OVER_FINISH, NEXT_MOVE, CYCLER_ID)]

#continue checking existing results based on next and follwing turn

still_missing <- joined_known_dods#[is.na(TURNS_TO_FINISH)]
still_missing[, c("TURNS_DOD", "MOVES_DOD") := as.character(NA)]
still_missing[, c("TURNS_DOD", "MOVES_DOD") := tstrsplit(DRAW_ODDS, ";", fixed = TRUE)]
still_missing[, c("FIRST_MOVE_DOD", "SECOND_MOVE_DOD") := as.character(NA)]
still_missing[, max_dod_turns := str_count(DRAW_ODDS, "\\.") + 1]

still_missing[, c("FIRST_MOVE_DOD") :=  tstrsplit(MOVES_DOD, ".", fixed = TRUE)[[1]]]
still_missing[max_dod_turns == 2, c("SECOND_MOVE_DOD") :=  tstrsplit(MOVES_DOD, ".", fixed = TRUE)[[2]]]

expand_first <- still_missing[, .(NEXT_MOVE = unlist(tstrsplit(FIRST_MOVE_DOD, "", type.convert = TRUE))),
                          by = .(max_dod_turns, DECK_LEFT, TRACK_LEFT, DRAW_ODDS, SECOND_MOVE_DOD,
                                 TURNS_TO_FINISH, SLOTS_OVER_FINISH, NEW_GAME_SLOT_ID, MOVEMENT, CYCLER_ID)]

expand_second <- expand_first[, .(FOLLOWING_MOVE = as.integer(unlist(tstrsplit(SECOND_MOVE_DOD, "", type.convert = TRUE)))),
                           by = .(max_dod_turns, DECK_LEFT, TRACK_LEFT, DRAW_ODDS, NEXT_MOVE,
                                  TURNS_TO_FINISH, SLOTS_OVER_FINISH, NEW_GAME_SLOT_ID, MOVEMENT, CYCLER_ID)]

#join known by next and following
ss_aggr <- ADM_OPTIMAL_MOVES_AGGR[, .(ttf_following = TURNS_TO_FINISH, DECK_LEFT, TRACK_LEFT, NEXT_MOVE,
                                      FOLLOWING_MOVE, slots_following = SLOTS_OVER_FINISH)]
joini_follow <- ss_aggr[expand_second, on = .(TRACK_LEFT, DECK_LEFT, NEXT_MOVE, FOLLOWING_MOVE)]


expand_second[, FOLLOWING_MOVE := NULL]
joini_follow_only_next <- ss_aggr[expand_second, on = .(TRACK_LEFT, DECK_LEFT, NEXT_MOVE)][max_dod_turns == 1]

both_res <- rbind(joini_follow, joini_follow_only_next)

nice_cols <- both_res[, .(TRACK_LEFT, DECK_LEFT, TURNS_TO_FINISH = ifelse(is.na(TURNS_TO_FINISH), ttf_following, TURNS_TO_FINISH),
                 SLOTS_OVER_FINISH = ifelse(is.na(SLOTS_OVER_FINISH), slots_following, SLOTS_OVER_FINISH), DRAW_ODDS, NEXT_MOVE, FOLLOWING_MOVE,
                 NEW_GAME_SLOT_ID, CYCLER_ID, MOVEMENT)]
nice_cols[,  ':=' (NEXT_MOVE = ifelse(is.na(TURNS_TO_FINISH), NA, NEXT_MOVE),
                   FOLLOWING_MOVE = ifelse(is.na(TURNS_TO_FINISH), NA, FOLLOWING_MOVE))]
dod_data_with_existing_results <- suppressWarnings(nice_cols[, .(NEXT_MOVE = max(NEXT_MOVE, na.rm = TRUE),
                                                FOLLOWING_MOVE = max(FOLLOWING_MOVE, na.rm = TRUE),
                                           TURNS_TO_FINISH = max(TURNS_TO_FINISH, na.rm = TRUE),
                                           SLOTS_OVER_FINISH = max(SLOTS_OVER_FINISH, na.rm = TRUE)), by = .(DRAW_ODDS,
                                                TRACK_LEFT, DECK_LEFT, NEW_GAME_SLOT_ID, MOVEMENT, CYCLER_ID)])
dod_data_with_existing_results[, ':=' (TURNS_TO_FINISH = ifelse(is.finite(TURNS_TO_FINISH), TURNS_TO_FINISH, NA),
                                       NEXT_MOVE = ifelse(is.finite(NEXT_MOVE), NEXT_MOVE, NA),
                                       FOLLOWING_MOVE = ifelse(is.finite(FOLLOWING_MOVE), FOLLOWING_MOVE, NA),
                                       SLOTS_OVER_FINISH = ifelse(is.finite(SLOTS_OVER_FINISH), SLOTS_OVER_FINISH, NA))]
#print(dod_data_with_existing_results)

} else {
  dod_data_with_existing_results <- NULL
}
} else {
  dod_data_with_existing_results <- NULL
}

} else {
  dod_data_with_existing_results <- NULL
}

clean_join_known <- join_known[, .(TURNS_TO_FINISH, SLOTS_OVER_FINISH, DRAW_ODDS, DECK_LEFT, NEXT_MOVE, TRACK_LEFT, NEW_GAME_SLOT_ID,
                                   FOLLOWING_MOVE, MOVEMENT, CYCLER_ID)]

dod_and_normal <- rbind(clean_join_known, dod_data_with_existing_results)[, .N, by = .(TURNS_TO_FINISH,
                                                                                       SLOTS_OVER_FINISH,
                                                                                       DRAW_ODDS,
                                                                                       DECK_LEFT,
                                                                                       TRACK_LEFT,
                                                                                        MOVEMENT,
                                                                                       CYCLER_ID,
                                                                                       NEXT_MOVE, NEW_GAME_SLOT_ID,
                                                                                       FOLLOWING_MOVE)][, N := NULL]
#dod_and_normal[, N := NULL]


#no draw odds in public information

#join known results

dod_and_normal[, IS_FINISHED := ifelse(TRACK_LEFT %in% c("", "N"), 1, 0)]

# if (calc_ttf == 0) {



# join_known <- ADM_OPTIMAL_MOVES_AGGR[dod_and_normal, on = .(TRACK_LEFT, DECK_LEFT, DRAW_ODDS)]
# join_known[, row_id_calc := NULL]
cases <- dod_and_normal[IS_FINISHED == 0, .N]
cover <- dod_and_normal[IS_FINISHED == 0 & !is.na(TURNS_TO_FINISH), .N]
pct <- round(cover/cases, 2)
dod_cases <-  dod_and_normal[IS_FINISHED == 0 & DRAW_ODDS != "", .N]
dod_cover <- dod_and_normal[IS_FINISHED == 0 & !is.na(TURNS_TO_FINISH)  & DRAW_ODDS != "" , .N]
dod_pct <- round(dod_cover / dod_cases, 2)
print(paste0(cases, " Cases and ", cover, " covered. ", dod_cases, " Dod cases and ", dod_cover, " covered"))
added_ttf <- add_ttf_multicore(con, dod_and_normal[is.na(TURNS_TO_FINISH) & IS_FINISHED == 0], pre_aggr_game_status_no_list)

dod_and_normal <- update_dt_values(dod_and_normal, added_ttf, c("DRAW_ODDS", "TRACK_LEFT", "DECK_LEFT"),
                 c("TURNS_TO_FINISH", "SLOTS_OVER_FINISH", "NEXT_MOVE"))

#dcastaa tulokset by draw_odds

dod_and_normal[, USED_DRAW_ODDS := ifelse(DRAW_ODDS != "", "_DOD", "")]

backup_dod_row <- data.table(TRACK_LEFT = -1, DRAW_ODDS = "DELME", USED_DRAW_ODDS = "_DOD")

fill_bu <- rbind(dod_and_normal, backup_dod_row, fill = TRUE)
problem_rows <- dod_and_normal[, .N, by = .(IS_FINISHED, TRACK_LEFT, DECK_LEFT, NEW_GAME_SLOT_ID, CYCLER_ID,
                                            MOVEMENT)][N > 2 , N]
if (length(problem_rows) > 0) {
  browser()
}
dcastaa_raw <- dcast.data.table(fill_bu, formula = IS_FINISHED + TRACK_LEFT + DECK_LEFT + NEW_GAME_SLOT_ID + MOVEMENT + CYCLER_ID ~ USED_DRAW_ODDS, sep = "", value.var = c("TURNS_TO_FINISH", "SLOTS_OVER_FINISH",
                                                                                                                                "NEXT_MOVE", "FOLLOWING_MOVE", "DRAW_ODDS"))
dcastaa <- dcastaa_raw[!is.na(DRAW_ODDS)]

finish_slot <- pre_aggr_game_status_no_list[FINISH == 1, GAME_SLOT_ID]
dcastaa[is.na(TURNS_TO_FINISH) & IS_FINISHED == 1, ':=' (TURNS_TO_FINISH = 0, SLOTS_OVER_FINISH = NEW_GAME_SLOT_ID - finish_slot, NEXT_MOVE = 5)]
# join_known[is.na(TURNS_TO_FINISH), c("TURNS_TO_FINISH", "SLOTS_OVER_FINISH", "NEXT_MOVE")
#            := (finish_turns_db(con, TRACK_LEFT, DECK_LEFT, pre_aggr_game_status_no_list, NEW_GAME_SLOT_ID,
#                                                                                    draw_odds_raw_data = DRAW_ODDS, save_to_DB = TRUE)),
#                   by = .(NEW_GAME_SLOT_ID, TRACK_LEFT, DECK_LEFT, DRAW_ODDS)]
# } else {
#
#   join_known <- join_track_left
#   join_known[, ':=' (TURNS_TO_FINISH = 15 - calc_ttf, SLOTS_OVER_FINISH = 1, NEXT_MOVE = 0)]
#   }
#if (join_known[, min(TURNS_TO_FINISH)] == 1) {browser()}



join_back_to_data_with_cyclers <- dcastaa[join_track_left, on = .(TRACK_LEFT, DECK_LEFT, NEW_GAME_SLOT_ID, MOVEMENT, CYCLER_ID)]

  join_to_combinations <- join_back_to_data_with_cyclers[join_curr_pos, on = .(NEW_GAME_SLOT_ID, CYCLER_ID, MOVEMENT)]
 # join_to_combinations <- join_known[jcp, on = .(NEW_GAME_SLOT_ID, CYCLER_ID, MOVEMENT)]
  #join_to_combinations <- join_known[join_curr_pos, on = .(NEW_GAME_SLOT_ID, CYCLER_ID, MOVEMENT), allow.cartesian =TRUE]



  #(#DIST_TO_TEAM = 1 - 1 / (floor(abs(sum(negative_new_game_slot_id)) / 1.1) + 1.5) ^ 2,


  # join_track_left[, DIST_TO_TEAM2 := min(max(dist_to_team_uncapped, 1) - 1, 2), by = .(case_id, TEAM_ID, CYCLER_ID)]



  #FINISH SCORE
  finish_slot <- pre_aggr_game_status_no_list[FINISH == 1, GAME_SLOT_ID]


#first sort fo get correct moves, cyclers order


  setorder(join_to_combinations, case_id, CYCLER_ID)
  join_to_combinations[, ':='  (MOVES = paste0(MOVEMENT, collapse = "_"),
                                CYCLERS = paste0(CYCLER_ID, collapse = "_")
                                ) , by = .(case_id, TEAM_ID)]




  #then sorts cyclers by case_id and new_square
  setorder(join_to_combinations, case_id, -NEW_SQUARE)
  #  join_track_left[, ':=' (MY_TEAM_ROW = ifelse(CYCLER_ID %in% smart_cycler_ids, 1,NA))]
  #join_track_left[, ':=' (MY_TEAM_MIN_TTF = min(TURNS_TO_FINISH * MY_TEAM_ROW, na.rm = TRUE)), by = case_id]
  # join_track_left[, ':=' (RELEVANT_OPPONENT = MY_TEAM_MIN_TTF >= (TURNS_TO_FINISH - 1)), by = case_id]
  join_to_combinations[, ':=' (CYCLER_MEAN_TTF = mean(TURNS_TO_FINISH, na.rm = TRUE)), by = .(CYCLER_ID)]
  join_to_combinations[, ':=' (CYCLER_MEAN_SOF = mean(SLOTS_OVER_FINISH, na.rm = TRUE)), by = .(CYCLER_ID)]



  join_to_combinations[, ':=' (SLOTS_PROGRESSED = NEW_GAME_SLOT_ID - curr_pos)]
  join_to_combinations[, ':=' (
    MOVE_ORDER = seq_len(.N),
    MOVE_DIFF_RELATIVE = MOVE_DIFF - (sum(MOVE_DIFF) - MOVE_DIFF) / (.N - 1)
  ), by = case_id]

  join_to_combinations[, ':=' (MOVE_DIFF_RELATIVE = ifelse(is.na(MOVE_DIFF_RELATIVE), 1, MOVE_DIFF_RELATIVE))]
  join_to_combinations[, ':=' (FINISH_ESTIMATE = MOVE_ORDER / 100 + TURNS_TO_FINISH - pmin(SLOTS_OVER_FINISH, 4) / 5)]
  setorder(join_to_combinations, case_id, FINISH_ESTIMATE)
  join_to_combinations[, FINISH_RANK_ALL := length(finished_cyclers) + seq_len(.N), by = case_id]
  join_to_combinations[, FINISH_RANK := 4 - pmin(FINISH_RANK_ALL, 4), by = case_id]
  setorder(join_to_combinations, case_id, -NEW_SQUARE)

  join_to_combinations[, ':=' (FINISH_ESTIMATE_MEAN = mean(FINISH_ESTIMATE)), by = .(CYCLER_ID)]
  join_to_combinations[, OVER_FINISH := pmax(NEW_GAME_SLOT_ID - finish_slot, 0)]


  #aggr_cyc <- STG_CYCLER[, .( CYCLERS = paste0(CYCLER_ID, collapse = "_")), by = TEAM_ID]
   #result <- aggr_cyc[join_to_combinations, on = "TEAM_ID"]
   result <- join_to_combinations
  return(result)

}
