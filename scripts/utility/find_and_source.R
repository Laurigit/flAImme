#find_and_source
#source_list <- c("STAT_VOITTOENNUSTE", "ölölkj")
find_and_source <- function(source_list, used_env = globalenv()) {
  #find all files
  #load("shiny_env.R")
  #used_env <- parent.frame()
  filenames <- data.table(filename = list.files(recursive = TRUE))
  source_to_table <- data.table(source_nm= source_list)
  source_to_table[, rivi := seq_len(.N)]
  source_to_table[, ':=' (source_found = grepl(source_nm,
                                             paste(filenames[, filename], collapse = "|"))),
                  by = rivi]
  source_files_not_found <- source_to_table[source_found == FALSE, source_nm]
  filenames[,  ':=' (match_nm = grepl(paste(source_list, collapse = "|"), filename))]
  filenames[, file_ending :=  toupper(str_sub(filename, -2, -1))]
  for(source_loop in filenames[match_nm == TRUE & file_ending == ".R", filename]) {
  #  print(paste0("Sourcing ", source_loop))
    source(source_loop, local = used_env, encoding="utf-8")
  #  print(paste0("Sourced ", source_loop))
  }
  if(length(source_files_not_found) > 0) {
    warning(source_files_not_found)
    return(source_files_not_found)
  }
}
