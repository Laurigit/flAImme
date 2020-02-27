
required_data(c("ADM_OPTIMAL_MOVES", "ADM_AI_CONF"))
shinyServer(function(input, output, session) {


  react_status <- reactiveValues(phase = 0,
                                 cycler_in_turn = 0,
                                 action_data = NULL,
                                 turn = 0,
                                 last_played_card = 0,
                                 first_cycler = 0,
                                 game_status = 0,
                                 deck_status = 0,
                                 precalc_track_agg = 0,
                                 ctM_data = 0,
                                 AI_team = 0,
                                 range_joined_team = 0,
                                 game_phase = 0)

  sourcelist <- data.table(polku = c(dir("./scripts/", recursive = TRUE)))
  sourcelist[, rivi := seq_len(.N)]
  suppressWarnings(sourcelist[, kansio := strsplit(polku, split = "/")[[1]][1], by = rivi])
  sourcelist <- sourcelist[!grep("load_scripts.R", polku)]
  sourcelist[, kansio := ifelse(str_sub(kansio, -2, -1) == ".R", "root", kansio)]


  input_kansio_list <- c(
    "tab"
  )
  for(input_kansio in input_kansio_list) {
    dir_list <- sourcelist[kansio == input_kansio, polku]
    for(filename in dir_list) {
      result = tryCatch({
        print(paste0("sourced ", filename))
        source(paste0("./scripts/", filename), local = TRUE)
      }, error = function(e) {
        print(paste0("error in loading file: ", filename))
      })
    }
  }


})
