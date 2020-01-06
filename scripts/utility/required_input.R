required_input <- function(input_nm, list_nm_vector = NULL, default_value_vector) {
 # https://stackoverflow.com/questions/32231835/assign-element-to-list-in-parent-frame
  
  if(!exists(input_nm,
             envir = parent.frame())) {
    assign(input_nm,
           default_value_vector,
           envir = parent.frame())
    if(!is.null(list_nm_vector)) {
    counter <- 0
    for (list_nm in list_nm_vector){
      counter <- counter + 1
      default_value <- default_value_vector[[counter]]
    suppressWarnings(
    eval(parse(text = sprintf("%s$%s <- %d", input_nm, list_nm, default_value)), envir = parent.frame())
    )
    }
    }
  }
  
}
