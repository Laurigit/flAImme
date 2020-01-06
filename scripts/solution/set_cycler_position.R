#set_cycler_position

set_cycler_position <- function(input_cycler_id, slot, lane, input_track_table) {
  input_track_table[CYCLER_ID == input_cycler_id, CYCLER_ID := 0]
  input_track_table[GAME_SLOT_ID == slot & LANE_NO == lane, CYCLER_ID := input_cycler_id]
  return(input_track_table)
}
