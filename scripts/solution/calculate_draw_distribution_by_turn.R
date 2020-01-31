
calculate_draw_distribution_by_turn <- function(cycler_id, current_decks_input, how_many_cards = 4) {

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
  #curr_deck_cards[1:12, Zone := "Recycle"]

  #check if enough cards
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


  }


 odds_limit <- 0.4
 #aggr  <- total_options[, .(count = .N), by = .(certain, draw_round_column, MOVEMENT)]#

total_options[, count := 1] #used to enable summing each aggregation
 for (odds_aggr_loop in (1:(total_options[, max(draw_round_column)]))) {
   total_options <- calc_odds(total_options, odds_limit, how_many_cards)
 }

 sscols_res <- total_options[,. (Turn_to_Draw = draw_round_column, MOVEMENT, prob = calc_phase_one)]
 sscols_res[is.nan(prob), prob := 1]
 aggr_res <- sscols_res[, .(prob = max(prob)), by = .(Turn_to_Draw, MOVEMENT)]
 sorted_res <- aggr_res[order(Turn_to_Draw, MOVEMENT)]

 return(sorted_res)
}

calc_odds <- function(total_options, odds_limit, how_many_cards)  {

  aggr  <- total_options[, .(count = sum(count)), by = .(certain, draw_round_column, MOVEMENT)]#

  aggr[, group_sum := sum(count), by= .(certain, draw_round_column) ]
  aggr[draw_round_column == 1 & certain < 100 & certain > 0, calc_phase_one := 1 - dhyper(certain, group_sum - count,  count,  certain)]
  aggr[certain == 100, calc_phase_one := 1]
  aggr[ certain != 100, calc_phase_one := 1- dhyper(how_many_cards, group_sum - count, count, how_many_cards)]
  aggr[calc_phase_one < odds_limit, draw_round_column := draw_round_column + 1]
  aggr[calc_phase_one < odds_limit & certain != 100, certain := -1]


  return(aggr)
}

#cycler_id <- 1

#calculate_draw_distribution_by_turn(cycler_id, deck_status, how_many_cards = 4)
