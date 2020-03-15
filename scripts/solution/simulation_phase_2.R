
#liikuta vihut move _cycler
#jätä alkupaikat paikoilleen[ max 1 per tiimi]
#änge väliin

simulation_phase_2 <- function(game_status, phase_two_cyclers,

                              move_simul_data,
                                 return_pos_vec = FALSE) {

  cop_game <- copy(game_status)

  #IDEA INSTEAD OF COPY, set the cyclers to orig positions on track after done.
  move_order <- cop_game[CYCLER_ID > 0 & CYCLER_ID %in% phase_two_cyclers, .(CYCLER_ID, SQUARE_ID, GAME_SLOT_ID)][order(-SQUARE_ID)]


  for (cyc_loop in move_order[, CYCLER_ID]) {
    loop_movement <- move_simul_data[CYCLER_ID == cyc_loop, MOVEMENT]

    cop_game <- move_cycler(cop_game, cyc_loop, loop_movement, FALSE, FALSE, FALSE, FALSE)

  }

  #slipstream

  cop_game <- apply_slipstream(cop_game)


  if (return_pos_vec == TRUE) {

    #new_pos <- cop_game[CYCLER_ID > 0, .(CYCLER_ID, GAME_SLOT_ID)]

    #https://stackoverflow.com/questions/37878620/reorder-rows-in-data-table-in-a-specific-order
    #order the cyclers in the original sorted order that was in the inpt data
    #new_pos <- setorder(new_pos[, .r := order(-move_simul_data[, CYCLER_ID])], .r)[, .r := NULL]


    res <- cop_game[order(CYCLER_ID)][CYCLER_ID > 0, GAME_SLOT_ID]
  } else {
    res <- cop_game
  }
  return(res)
}

