
calculate_draw_distribution_by_turn <- function(cycler_id, current_decks_input, how_many_cards = 4, db_res = FALSE) {

  #case draw more than cards in deck
  #use only cards in deck until need to recycle

  #draw exactly rest of the deck4
  #until next shuffle, those cards are not available

  #draw less than deck
  #I know X cards for sure, and random from the rest. The X cards not used until reshuffle

  #I draw all the deck.
  #all random


  #1 check if cards left in deck
  #for_debug <- current_decks_input#[cycler_id == CYCLER_ID]

  #

  curr_deck_cards <- current_decks_input[cycler_id == CYCLER_ID]
  #if we have no cards left, it means we are going to draw an exhaust

  if (nrow(curr_deck_cards[Zone != "Removed"]) == 0) {
    curr_deck_cards <- data.table(CYCLER_ID = cycler_id,
                                  CARD_ID = 1,
                                  Zone = "Recycle",
                                  MOVEMENT = 2,
                                  row_id = 1)
  }

  #curr_deck_cards[1:12, Zone := "Recycle"]

  #check if enough cards in deck to draw!
  card_count <- nrow(curr_deck_cards[cycler_id == CYCLER_ID & Zone == "Deck"])


  stop_loop <- FALSE
  draw_round <- 0
  total_options <- NULL
  shuffle_count <- 0

  while(stop_loop == FALSE) {
    #try to draw for
    draw_round <- draw_round + 1
    card_count <- card_count - how_many_cards
    if (card_count <= 0 & draw_round == 1) {
      shuffle_count <- 1

      if (draw_round == 1) {
        certainty_pct <- 100

      } else {
        certainty_pct <- -1
        stop_loop <- TRUE


      }

      options_start <- curr_deck_cards[cycler_id == CYCLER_ID & Zone == "Deck",.(MOVEMENT)][, certain := certainty_pct]
      #shuffle
      curr_deck_cards[cycler_id == CYCLER_ID &Zone == "Deck", Zone := "Hand"]
      curr_deck_cards[cycler_id == CYCLER_ID &Zone == "Recycle", Zone := "Deck"]
      curr_deck_cards[cycler_id == CYCLER_ID &Zone == "Hand", Zone := "Recycle"]

      card_count <-  nrow(curr_deck_cards[cycler_id == CYCLER_ID & Zone == "Deck"])
      #count missing cards
      missing <- how_many_cards - nrow(options_start)
      if (missing > 0) {
      add_options <- curr_deck_cards[cycler_id == CYCLER_ID & Zone == "Deck", .(MOVEMENT)][, certain := missing ]
      options <- rbind(options_start, add_options)
      } else {
        options <- options_start
      }
    } else if (card_count > 0) {
      options <- curr_deck_cards[cycler_id == CYCLER_ID & Zone == "Deck", .(MOVEMENT)][, certain := -1]
    } else {
      stop_loop <- TRUE
      break()
    }
    options[, draw_round_column := draw_round]
    total_options <- rbind(total_options, options)
    if (draw_round == 2) {stop_loop <- TRUE}

  }


 odds_limit <- 0.35
 #aggr  <- total_options[, .(count = .N), by = .(certain, draw_round_column, MOVEMENT)]#

total_options[, count := 1] #used to enable summing each aggregation
loop_rounds <- total_options[, max(draw_round_column)]

 for (odds_aggr_loop in (1:loop_rounds)) {

   last_round <- FALSE
   if (odds_aggr_loop == loop_rounds) {
     last_round <- TRUE
   }
   total_options <- calc_odds(total_options, odds_limit, how_many_cards, last_round)
 }

 sscols_res <- total_options[,. (Turn_to_Draw = draw_round_column, MOVEMENT, prob = calc_phase_one)]
 sscols_res[is.nan(prob), prob := 1]
 aggr_res <- sscols_res[, .(prob = max(prob)), by = .(Turn_to_Draw, MOVEMENT)]

 sorted_res <- aggr_res[order(Turn_to_Draw, MOVEMENT)]

 #remove turns with only on option
 opt_count <- sorted_res[, .N, by = Turn_to_Draw]
 #leave at least one row
 #only take last row out if it contains only one row
 last_row <- sorted_res[, max(Turn_to_Draw)]
 last_group_row_count <- sorted_res[Turn_to_Draw == last_row, .N]
 if (last_group_row_count == 1 & last_row > 1){
   filtered_res <- sorted_res[Turn_to_Draw != last_row]
 } else {
   filtered_res <- sorted_res
 }

 #clear turns that have all options left
 # max_options <- curr_deck_cards[cycler_id == CYCLER_ID & Zone != "Removed", .N, by = MOVEMENT][, .N]
 # turn_options <- filtered_res[, .N, by = Turn_to_Draw]
 # turn_options[, remove_turn := N == max_options]
 # remove_these_turns <- turn_options[remove_turn == TRUE, Turn_to_Draw]
 second_filter_res <- filtered_res#[!Turn_to_Draw %in% remove_these_turns]



 if (db_res == TRUE) {

   movement_string <- paste0(second_filter_res[, paste0(MOVEMENT, collapse = ""), by = Turn_to_Draw][, V1], collapse = ".")
    db_vec <- paste0(paste0(second_filter_res[, Turn_to_Draw], collapse = ""), ";", movement_string)
    if (db_vec == ";") {db_vec <- ""}
    return(db_vec)
 } else {
   return(sorted_res)
  }

}

calc_odds <- function(total_options, odds_limit, how_many_cards,last_round)  {

  aggr  <- total_options[, .(count = sum(count)), by = .(certain, draw_round_column, MOVEMENT)]#

  aggr[, group_sum := sum(count), by= .(certain, draw_round_column) ]
  how_many_uncertain_left_d1 <- aggr[certain == 100, .N]
  aggr[draw_round_column == 1 & certain < 100 & certain > 0, calc_phase_one := 1 - dhyper(0, count, group_sum - count, how_many_cards - how_many_uncertain_left_d1)]
  aggr[certain == 100, calc_phase_one := 1]
  aggr[ certain < 0, calc_phase_one := 1- dhyper(0, count, group_sum - count, how_many_cards)]

  if (last_round == FALSE) {
    aggr[calc_phase_one < odds_limit, draw_round_column := draw_round_column + 1]

  }
  aggr[calc_phase_one < odds_limit & certain != 100, certain := -1]
  return(aggr)
}

#cycler_id <- 1

#calculate_draw_distribution_by_turn(cycler_id, deck_status, how_many_cards = 4)
