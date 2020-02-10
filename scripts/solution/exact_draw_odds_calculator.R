exact_draw_odds_calculator <- function(MOVEMENT_POOL, MOVEMENTS_wanted, MOVEMENTS_not_drawn) {
  #MOVEMENTS_wanted<- c(3,4)
  #MOVEMENTS_not_drawn <- 7
  #MOVEMENT_POOL <- c(3,3,3,4,4,5,6,7,8,8,8)

  dt_move <- data.table(MOVEMENT = MOVEMENT_POOL)
  dt_move[, WANTED := MOVEMENT %in% MOVEMENTS_wanted]
  dt_move[, NOT_WANTED := MOVEMENT %in% MOVEMENTS_not_drawn]

  #p(dont draw) * p(draw)
  white_balls <- dt_move[NOT_WANTED == TRUE, .N]
  black_balls <- dt_move[NOT_WANTED != TRUE, .N]
  pDont <- phyper(0, white_balls, black_balls, 4)

  #p do
  white_balls_do <- dt_move[WANTED == TRUE, .N]
  black_balls_do <- dt_move[WANTED == FALSE & NOT_WANTED == FALSE, .N]
  pDo <- 1 - phyper(0, white_balls_do, black_balls_do, 4)

  result <- pDo * pDont
 return(result)
}
