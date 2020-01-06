#function_vector <- c("func_voittoennuste", "func_draft_boosters")
required_functions <- function(function_vector, used_env = globalenv()) {
  #search all R scripts under working directory
 
  all_files <- dir(recursive = TRUE)
  r_files <- all_files[grep(".R", all_files)]
  matched_files <- r_files[unlist(lapply(function_vector, grep, r_files))]
  #source files
  for (loop_file in matched_files) {
    #parent.frame on environemnt, josta funktio kutsuttiin.
    source(loop_file, local = used_env)
  }
}
