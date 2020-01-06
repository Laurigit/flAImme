#SRC_TRACK

luettu <- dbSelectAll("TRACK", con) 

SRC_TRACK <- fix_colnames(luettu)
