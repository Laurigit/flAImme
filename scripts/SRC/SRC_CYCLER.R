#SRC_CYCLER

luettu <- dbSelectAll("CYCLER", con) 

SRC_CYCLER <- fix_colnames(luettu)
