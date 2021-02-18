
calculate_draw_distribution_by_turn_with_prio <- function(input_data, current_decks_input, how_many_cards = 4, db_res = FALSE) {


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

    #input_data
  copy_data <- copy(input_data)
  copy_data[, sort_order := seq_len(.N)]
  cycler_id <- input_data[, .N, by = OTHER_CYCLER][, OTHER_CYCLER]

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

    #check if enough cards
    card_count <- nrow(curr_deck_cards[cycler_id == CYCLER_ID & Zone == "Deck"])


    stop_loop <- FALSE
    draw_round <- 0
    total_options_wide <- NULL
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
      total_options_wide <- rbind(total_options_wide, options)


    }



    #aggr  <- total_options[, .(count = .N), by = .(certain, draw_round_column, MOVEMENT)]#

    total_options_wide[, count := 1] #used to enable summing each aggregation
    #join_prio
    ss_prio <- input_data[, .(MOVEMENT = OTHER_MOVE, PRIO)]


    join_prio <- ss_prio[total_options_wide, on = "MOVEMENT"][draw_round_column == 1]


      total_options <- calc_odds_prio(join_prio, odds_limit, how_many_cards)


    sscols_res <- total_options

    ss_sort_order <- copy_data[, .(MOVEMENT = OTHER_MOVE, sort_order)]
   join_sort <- sscols_res[ss_sort_order, on = "MOVEMENT"]
   join_sort[, PROB := ifelse(is.na(PROB), 0, PROB)]
   setorder(join_sort, sort_order)
    sorted_res <- join_sort[, PROB]


return(sorted_res)


}





calc_odds_prio <- function(total_options, odds_limit = 0, how_many_cards)  {

  aggr  <- total_options[draw_round_column == 1 & certain != 100, .(count = sum(count)), by = .(certain, draw_round_column, MOVEMENT, PRIO)]#

  setorder(aggr, draw_round_column , PRIO)
  aggr[, cumsum_count := cumsum(count)]
  aggr[, not_wanted_count := cumsum_count - count]
  aggr[, group_sum := sum(count)]
  how_many_uncertain_left_d1 <- total_options[certain == 100, .N]

  aggr[, pDont := phyper(0, not_wanted_count, group_sum - not_wanted_count, how_many_cards - how_many_uncertain_left_d1)]
  aggr[,  pDo := 1 - suppressWarnings(phyper(0, count, group_sum - count - not_wanted_count,  how_many_cards - how_many_uncertain_left_d1))]
  aggr[, pTot := pDont * pDo]
  aggr[, pTot := ifelse(is.na(pTot), 0, pTot)]

  #include the certain card with highest prio
  lowest_prio_certain <- total_options[total_options[draw_round_column == 1 & certain == 100, .I[which.min(PRIO)]]]
  certaion_prio <- min(lowest_prio_certain[, PRIO], 100)
  #remove exising low certain prio from aggr
  aggr_filtered <- aggr[PRIO != certaion_prio]
  append_to_aggr <- rbind(aggr_filtered, lowest_prio_certain, fill = TRUE)
  #this one gets rest of the probs

  append_to_aggr[, pTot_with_certaion := ifelse(PRIO < certaion_prio, pTot, 0)]
  free_prob_left <- 1 - append_to_aggr[, sum(pTot_with_certaion)]
  append_to_aggr[certain == 100, pTot_with_certaion := free_prob_left]
  result <- append_to_aggr[, .(MOVEMENT, PROB = pTot_with_certaion)]
  return(result)




}

#cycler_id <- 1

#calculate_draw_distribution_by_turn(cycler_id, deck_status, how_many_cards = 4)
