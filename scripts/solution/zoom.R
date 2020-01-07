zoom <- function(game_status) {
  min_cycler <- game_status[CYCLER_ID > 0, min(GAME_SLOT_ID)]
  max_cycler <- game_status[CYCLER_ID > 0, max(GAME_SLOT_ID)]
  res <- game_status[GAME_SLOT_ID >= min_cycler & GAME_SLOT_ID <= max_cycler + 10, .(CYCLERS = (paste0(CYCLER_ID, collapse = "-"))), by = .(GAME_SLOT_ID, PIECE_ATTRIBUTE, FINISH)]
 return(res)
  }
