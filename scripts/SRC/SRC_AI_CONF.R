#SRC_AI_CONF

luettu <- data.table(read_excel("./external_files/AI_conf.xlsx"))

SRC_AI_CONF <- fix_colnames(luettu)
