#add exhaustion

#curr_decks <- create_decks(input_STARTUP_DATA[, CYCLER_ID], ADM_CYCLER_DECK)
#cycler_id_vect <- c(1,2,3)
add_exhaustion <- function(cycler_id_vect, curr_decks, input_zone) {
  max_row_id <- (curr_decks[, max(row_id)] +1):(curr_decks[, max(row_id)]  + length(cycler_id_vect))
  new_row <- data.table(CYCLER_ID = cycler_id_vect, CARD_ID = 1, Zone = input_zone, row_id = max_row_id, MOVEMENT = 2)
  appendaa <- rbind(curr_decks, new_row)
  return(appendaa)


}
