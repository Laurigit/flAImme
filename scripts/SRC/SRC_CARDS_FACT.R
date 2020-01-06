#SRC_CARDS_FACT

luettu <- dbSelectAll("CARDS_FACT", con) 

SRC_CARDS_FACT <- fix_colnames(luettu)
