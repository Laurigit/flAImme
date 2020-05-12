create_other_moves <- function(all_moves, my_move_index) {

  rest_of_moves <- paste0(all_moves[[1]][-my_move_index], collapse = "_")
  return(rest_of_moves)
}
