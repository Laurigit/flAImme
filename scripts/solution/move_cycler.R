# required_data("STG_CYCLER")
# input_STARTUP_DATA <- data.table(CYCLER_ID = c(1,2,3,4,5,6),
#                                  PLAYER_ID = c(1,1,2,2,3,3),
#                                  exhaust = c(0, 0, 0, 0, 0, 0),
#                                  starting_row =  c(1,2,1, 3,2,3),
#                                   starting_lane = c(1, 1, 2, 1, 2, 2))
#  track <- 2


#game_status <- start_game(input_STARTUP_DATA, 2, STG_TRACK_PIECE, STG_TRACK)
#current_game_status <- game_status
#cycler_id <- 3
#movement <- 4
#move_cycler

move_cycler <- function(current_game_status_input, cycler_id, movement, slipstream = FALSE, ignore_block = FALSE, ignore_end_of_track = FALSE, return_numeric_position = FALSE) {
  if (return_numeric_position == TRUE) {
    current_game_status <- copy(current_game_status_input)
  } else {
    current_game_status <- current_game_status_input
  }

  current_position_info <- current_game_status[cycler_id == CYCLER_ID, .(GAME_SLOT_ID, PIECE_ATTRIBUTE, MINIMUM_MOVEMENT, MAXIMUM_MOVEMENT)]
  current_position <- current_game_status[cycler_id == CYCLER_ID, GAME_SLOT_ID]
  #check if started from ascend
  terrain <- current_position_info[, PIECE_ATTRIBUTE]
  track_min_movement <- current_position_info[, MINIMUM_MOVEMENT]
  adjusted_movement <- movement
  if (slipstream == FALSE) {
    adjusted_movement <- max(movement, track_min_movement)
  }

  #check if road contains mountains
  max_move <- current_position_info[GAME_SLOT_ID == current_position, max(MAXIMUM_MOVEMENT)]


    adjusted_movement <- min(adjusted_movement, max_move)



  #cant go out of track in finish
  is_finish <- current_game_status[GAME_SLOT_ID >= current_position & GAME_SLOT_ID <= (current_position + adjusted_movement), sum(FINISH)]

  if (is_finish > 0 & ignore_end_of_track == FALSE) {


    #check max movement available
    last_slot <- current_game_status[is.na(EXTRA), max(GAME_SLOT_ID)]
    max_movement <- last_slot - current_position
    adjusted_movement <- min(max_movement, adjusted_movement)
  }



 if (ignore_block == FALSE) {
 new_square <- current_game_status[GAME_SLOT_ID <= (adjusted_movement + current_position) & CYCLER_ID == 0, max(SQUARE_ID)]

 new_slot <- current_game_status[GAME_SLOT_ID <= (adjusted_movement + current_position) & CYCLER_ID == 0, max(GAME_SLOT_ID)]
 } else {
   new_square <- current_game_status[GAME_SLOT_ID <= (adjusted_movement + current_position), max(SQUARE_ID)]
   new_slot <- current_game_status[GAME_SLOT_ID <= (adjusted_movement + current_position), max(GAME_SLOT_ID)]
   #check if it is occupied and who occupies it
   slot_occupied_cycler <- current_game_status[SQUARE_ID == new_square, CYCLER_ID]
   #if is occupied, push it

   if (slot_occupied_cycler > 0) {
     current_game_status <- push_cycler_down(current_game_status, slot_occupied_cycler)
   }

 }


 if (return_numeric_position == TRUE) {
   result <- new_slot
 } else {
   #clear old position
   current_game_status[cycler_id == CYCLER_ID, CYCLER_ID := 0]
   #move cyclers to right
      #check if there are cyclers on the slot where this one left with higher lane no
      count_cyclers_orig_slot <- current_game_status[GAME_SLOT_ID == current_position]



   current_game_status[SQUARE_ID == new_square, CYCLER_ID := cycler_id]
   result <- current_game_status
 }
  return(result)
}
