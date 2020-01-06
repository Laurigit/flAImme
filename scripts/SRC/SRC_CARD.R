#SRC_CARD

luettu <- dbSelectAll("CARD", con) 

SRC_CARD <- fix_colnames(luettu)
