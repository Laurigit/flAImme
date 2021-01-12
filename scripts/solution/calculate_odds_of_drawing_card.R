calculate_odds_of_drawing_card <- function(input_deck_left, target_card) {
  # input_deck_left <- "77666555444333"
  # target_card <- "7"
    #p do
    white_balls_do <- str_count(input_deck_left, as.character(target_card))
    black_balls_do <- nchar(input_deck_left) -  white_balls_do
    pDo <- 1 - suppressWarnings(phyper(0, white_balls_do, black_balls_do, 4))

    result <- pDo
    return(result)

}
