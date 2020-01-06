#SRC_CYCLER_TYPE

luettu <- dbSelectAll("CYCLER_TYPE", con) 

SRC_CYCLER_TYPE <- fix_colnames(luettu)
