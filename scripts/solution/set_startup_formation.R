set_startup_formation <- function(cycler_vector, start_format) {

  if (start_format == "RANDOM") {
    cycler_ids <- sample(cycler_vector)
  } else if (start_format == "REVERSED") {
    if (!is.null(total_winner)) {
      cycler_ids <- total_winner[, mean(POSITION), by = CYCLER_ID][order(-V1)][, CYCLER_ID]
    } else {
      cycler_ids <- sample(cycler_vector)
    }
  }
  return(cycler_ids)
}
