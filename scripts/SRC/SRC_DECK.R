#SRC_DECK

luettu <- dbSelectAll("DECK", con) 

SRC_DECK <- fix_colnames(luettu)
