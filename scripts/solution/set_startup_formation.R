set_startup_formation <- function(cycler_vector, start_format) {

  if (start_format == "RANDOM") {
    cycler_ids <- sample(cycler_vector)
  } else if  (start_format == "REVERSED_BASED_ON_POS")  {
    if (!is.null(total_winner)) {

      order_by_last_first <- total_winner[, mean(POSITION), by = CYCLER_ID][order(-V1)][, CYCLER_ID]
      total_winner[, mean(POSITION), by = CYCLER_ID][order(-V1)]
       total_winner[, mean(POSITION), by = .(START_POSITION)][order(-V1)]
      best_start_pos <- total_winner[, mean(POSITION), by = .(START_POSITION)][order(-V1)][, START_POSITION]
      cycler_ids <- best_start_pos[match(order_by_last_first, sort(order_by_last_first))]

    } else {
      cycler_ids <- sample(cycler_vector)
    }

  } else if (start_format == "REVERSED") {
    if (!is.null(total_winner)) {
      cycler_ids <- total_winner[, mean(POSITION), by = CYCLER_ID][order(-V1)][, CYCLER_ID]
    } else {
      cycler_ids <- sample(cycler_vector)
    }
  }
  return(cycler_ids)
}
