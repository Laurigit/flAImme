required_data(c("STG_CYCLER", "STG_DECK", "STG_CYCLER_TYPE", "STG_CARD"))

#ADM_CYCLER_DECK
joinaa <- STG_CYCLER[STG_CYCLER_TYPE, on = "CYCLER_TYPE_ID"]
join_deck <- joinaa[STG_DECK, on = "DECK_ID", allow.cartesian = TRUE]
ADM_CYCLER_DECK <-  STG_CARD[join_deck, on = "CARD_ID"]
