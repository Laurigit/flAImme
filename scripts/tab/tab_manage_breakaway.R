#tab_manage_breakaway

observe({
#THIS ONE MONITORS IF WE HAVE FINISHED BREAKAWAY BETTING
req(srv$gs_simple)

  req(input$join_tournament)
####DEP
breakaway_bet_data()
########DEPD#############


command_rows <- command_data()[TOURNAMENT_NM == input$join_tournament, .(COMMAND, COMMAND_ID)]
first_command <- command_rows[1, COMMAND]
if (is.na(first_command)) {
  first_command <- ""
}
if (first_command == "BREAKAWAY_DONE") {
#how many inputs needed?
new_game_found <- tournament_data_reactive()



if (nrow(new_game_found) > 0) {
  max_row <- new_game_found[, .N]
  tour_name <- new_game_found[max_row, TOURNAMENT_NM]



  player_count <- new_game_found[TOURNAMENT_NM == tour_name & GAME_ID == srv$game_id, .N] / 2
  ba_data <-   breakaway_bet_data()
  better_players <- ba_data[TOURNAMENT_NM == tour_name & GAME_ID == srv$game_id & SECOND_BET > 0 & FIRST_BET > 0, .N]

  missing_bets <- player_count - better_players
  #check that turn is -1 and everyone has bet, then execute breakaway
  turn_is_minus_one <- FALSE

  if (length(srv$gs_simple[TOURNAMENT_NM == tour_name, .N]) > 0) {
    if (srv$gs_simple[TOURNAMENT_NM == tour_name, max(TURN_ID)] == -1) {
      turn_is_minus_one <- TRUE
    }
  }

  if (missing_bets == 0 &   turn_is_minus_one) {
    #move cycers to breakaway
    #who won?

    #tie breakers are solved in favour of who is back in the grid

    ss_cyc <- srv$gs_simple[order(SQUARE_ID)][, .(CYCLER_ID)]
    ss_cyc[, tie_breaker := seq_len(.N)]

    winner_data <- ba_data[TOURNAMENT_NM == tour_name & GAME_ID == srv$game_id, .(CYCLER_ID, TOTAL_BET = SECOND_BET + FIRST_BET, FIRST_BET, SECOND_BET)][order(-TOTAL_BET)]
    join_tiebreaker <- ss_cyc[winner_data, on = "CYCLER_ID"]

    #if there are 5 or 6 cyclers, then 2 breaks, otherwise 1
    count_cyc <- nrow(ss_cyc)
    if (count_cyc > 8) {
      break_away_count <- 2
    } else {
      break_away_count <- 1
    }
    winners <- join_tiebreaker[order(-TOTAL_BET, tie_breaker)][1:break_away_count]


    lane_counter <- 0
    #create game status if missing
    if (is.null(srv$game_status)) {

      srv$game_status <- create_game_status_from_simple(srv$gs_simple, srv$track_id_input, STG_TRACK, STG_TRACK_PIECE)
    }

    for (winner_loop in winners[, CYCLER_ID]) {
      lane_counter <- lane_counter + 1

      srv$game_status <- set_cycler_position(winner_loop, 10, lane_counter, srv$game_status)
    }
    simple_gs <- srv$game_status[CYCLER_ID > 0, .(CYCLER_ID, SQUARE_ID)]
    simple_gs[, TOURNAMENT_NM := tour_name]
    simple_gs[, GAME_ID := srv$game_id]
    simple_gs[, TURN_ID := 0]

    srv$gs_simple <- simple_gs

    #then update deck_status

    deck_status <- dbSelectAll("DECK_STATUS", con )[TOURNAMENT_NM == tour_name & GAME_ID == srv$game_id]
    for (winner_loop in winners[, CYCLER_ID]) {

      deck_status <- play_card(cycler_id = winner_loop,
                               card_id = winners[CYCLER_ID == winner_loop, FIRST_BET],
                               current_decks_inpu = deck_status,
                               game_id = -1,
                               turn_id = 0,
                               con = NULL,
                               force = TRUE)
      deck_status <- play_card(cycler_id = winner_loop,
                               card_id = winners[CYCLER_ID == winner_loop, SECOND_BET],
                               current_decks_inpu = deck_status,
                               game_id = -1,
                               turn_id = 0,
                               con = NULL,
                               force = TRUE)
      #exh
      deck_status <-  add_exhaustion(winner_loop, deck_status, "Deck")
      deck_status <- add_exhaustion(winner_loop, deck_status, "Deck")
    }
    info_joined <- copy(deck_status)
    info_joined[, ':=' (GAME_ID = srv$game_id,
                        TOURNAMENT_NM = tour_name,
                        TURN_ID = 0,
                        HAND_OPTIONS = 0)]
    dbWriteTable(con, "DECK_STATUS", info_joined, append = TRUE, row.names = FALSE)
    dbWriteTable(con, "GAME_STATUS", simple_gs, row.names = FALSE, append = TRUE)


    rel_com_id <- command_rows[1, COMMAND_ID]
    dbQ(paste0('DELETE FROM CLIENT_COMMANDS WHERE COMMAND_ID = ', rel_com_id), con)

    command <- data.table(TOURNAMENT_NM = input$join_tournament, COMMAND = "START")
    dbIns("CLIENT_COMMANDS", command, con)

}

  }
} else {
  NULL
}

})
