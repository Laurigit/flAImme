zoom <- function(game_status, horizon = 10, max_lag = 10) {
  min_cycler <- game_status[CYCLER_ID > 0, min(GAME_SLOT_ID)]
  max_cycler <- game_status[CYCLER_ID > 0, max(GAME_SLOT_ID)]

  res <- game_status[GAME_SLOT_ID >= max(max_cycler - max_lag, min_cycler) & GAME_SLOT_ID <= max_cycler + horizon, .(CYCLERS = (paste0(CYCLER_ID, collapse = "-"))), by = .(GAME_SLOT_ID, PIECE_ATTRIBUTE, FINISH)]
 return(res)
  }
