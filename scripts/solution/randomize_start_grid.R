randomize_start_grid <- function(game_status) {


  tracki <-  game_status
  start_row <- tracki[START == 1, max(GAME_SLOT_ID)]
  #set positions
  updated_status <- tracki

  start_row_width <- tracki[START == 1, .N]

  cycler_vec <- tracki[CYCLER_ID > 0, CYCLER_ID]
  cycler_dt <- data.table(CYCLER_ID = sample(cycler_vec))

  cycler_dt[, grid_order := seq_len(.N)]
  cycler_dt[, starting_row := ceiling(grid_order / start_row_width)]
  cycler_dt[, starting_lane := seq_len(.N), by = starting_row]

  for(loop in cycler_vec) {
    row_data <- cycler_dt[CYCLER_ID == loop]
    updated_status <-   set_cycler_position(row_data[, CYCLER_ID],
                                            start_row + 1- row_data[, starting_row],
                                            row_data[, starting_lane],
                                            updated_status)
  }

  return(updated_status)
}
