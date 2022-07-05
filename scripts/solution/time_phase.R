time_phase <- function(phase_name, IS_START, total_current = NULL, game_id, turn_id) {

  if (IS_START == TRUE) {
    start_row <- data.table(GAME_ID = game_id, TURN_ID = turn_id, PHASE = phase_name, DURATION = as.numeric(Sys.time()))
    result <- rbind(start_row, total_current)
  } else {

    start_time <- total_current[PHASE == phase_name & TURN_ID == turn_id & game_id == GAME_ID, DURATION]
    duration <- as.numeric(Sys.time()) - start_time
    end_row <- data.table(GAME_ID = game_id, TURN_ID = turn_id, PHASE = phase_name, DURATION = duration)
    remove_start <- total_current[!(PHASE == phase_name  & TURN_ID == turn_id & game_id == GAME_ID)]
    result <- rbind(remove_start, end_row)

  }
  return(result)
}
