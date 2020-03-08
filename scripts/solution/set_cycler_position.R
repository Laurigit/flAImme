#set_cycler_position

set_cycler_position <- function(input_cycler_id, slot, lane = NULL, input_track_table) {
  input_track_table[CYCLER_ID == input_cycler_id, CYCLER_ID := 0]
  # if lane is null, find free lane
  if (is.null(lane)) {

    free_square <-  input_track_table[GAME_SLOT_ID == slot & CYCLER_ID == 0, max(SQUARE_ID)]
    if (is.infinite(free_square)){

      warning("NO ROOM TO SET CYCLER POSITION at set_cycler_position")
    }
    lane <- game_status[SQUARE_ID == free_square, LANE_NO]
  }

  #check if new slot is free
  new_square <- input_track_table[GAME_SLOT_ID == slot & LANE_NO == lane, SQUARE_ID]
  #check if it is occupied and who occupies it
  slot_occupied_cycler <- input_track_table[SQUARE_ID == new_square, CYCLER_ID]
  #if is occupied, push it
  if (slot_occupied_cycler > 0) {
    input_track_table <- push_cycler_down(input_track_table, slot_occupied_cycler)
  }

  input_track_table[GAME_SLOT_ID == slot & LANE_NO == lane, CYCLER_ID := input_cycler_id]
  return(input_track_table)
}
