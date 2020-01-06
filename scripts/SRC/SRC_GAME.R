#SRC_GAME

luettu <- dbSelectAll("GAME", con) 

SRC_GAME <- fix_colnames(luettu)
