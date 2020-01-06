#SRC_TRACK_PIECE

luettu <- dbSelectAll("TRACK_PIECE", con) 

SRC_TRACK_PIECE <- fix_colnames(luettu)
