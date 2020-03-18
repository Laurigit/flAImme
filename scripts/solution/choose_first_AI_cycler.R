choose_first_AI_cycler <- function(team_id, game_status, strategy, STG_CYCLER) {
  #my cyclers still playing
  my_team_cyclers <- STG_CYCLER[TEAM_ID == team_id, CYCLER_ID]
  #still playing
  #finish
  finish_slot <- game_status[FINISH == 1, max(GAME_SLOT_ID)]
  playing_cyc <- game_status[CYCLER_ID %in% my_team_cyclers & GAME_SLOT_ID < finish_slot, CYCLER_ID]
  if (length(playing_cyc) > 0) {
    if (strategy ==  "random") {
      result <- sample(playing_cyc, 1)
    } else if (strategy == "leading") {
      posits <- game_status[CYCLER_ID %in% playing_cyc][order(-SQUARE_ID)][, CYCLER_ID][1]
    }
  } else {
    result <- NA
  }


}
