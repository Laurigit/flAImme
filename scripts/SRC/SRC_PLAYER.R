#SRC_PLAYER

luettu <- dbSelectAll("PLAYER", con) 

SRC_PLAYER <- fix_colnames(luettu)
