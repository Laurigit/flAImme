exact_draw_odds_outer <- function(prio_group_target, draw_odds_data) {
  #prio_group, N,
  #prio_group_target <- 1
  # draw_odds_data <- data.table(prio_group = c(1, 2, 3), N = c(3, 2, 2))
  all_moves <- draw_odds_data[rep(1:.N,N)][,Indx:=1:.N,by=prio_group][, prio_group]
  not_wanted <- draw_odds_data[prio_group < prio_group_target, unique(prio_group)]
  if (length(all_moves) >= 4) {


  accepted_moves <- prio_group_target

  res <- exact_draw_odds_calculator(all_moves, accepted_moves, not_wanted)
  if (is.nan(res)) {
    res <- 0
  }
  } else {
    if (length(not_wanted) > 0) {
      res <- 0
    } else {
      res <- 1
    }
  }
  return(res)
}
