#SRC_TEAM

luettu <- dbSelectAll("TEAM", con) 

SRC_TEAM <- fix_colnames(luettu)
