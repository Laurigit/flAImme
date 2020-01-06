#SRC_MOVE_FACT

luettu <- dbSelectAll("MOVE_FACT", con) 

SRC_MOVE_FACT <- fix_colnames(luettu)
