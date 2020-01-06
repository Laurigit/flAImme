#ADM_DI_HIERARKIA
input_hakemistot <- c("SRC", "STG", "INT", "ADM", "STAT")
tiedostot_Dt <- data.table(tiedostot = dir("./scripts",recursive = TRUE))
tiedostot_Dt[, rivi := seq_len(.N)]
tiedostot_Dt[, alikansio := strsplit(tiedostot, "/")[[1]][1], by = rivi]
include_only_selected <- tiedostot_Dt[alikansio %in% input_hakemistot]

rivi_lkm <- nrow(include_only_selected)
total_result_data <- NULL
for (loop_row in 1:rivi_lkm) {
  kierrosData <- include_only_selected[loop_row]

result <- suppressWarnings(readtext(paste0("./scripts/",kierrosData[, tiedostot])))

split<- strsplit(x = result$text, split = "\n")
dt_split <- data.table(sana = split[[1]])
dt_split[, find_req_data := grepl("required_data", sana)]
ssrows <- dt_split[find_req_data == TRUE]
if(nrow(ssrows) > 0) {
ssrows[, taulut := ex_between(text = sana, left = '("', right = '")', fixed = TRUE, extract = TRUE)]
ssrows[, putsaa := gsub("[^[:alnum:]_ ]", "", taulut)]

palottele <- data.table(PARENT_TABLE_NM = (strsplit(x = ssrows[,putsaa], split = " "))[[1]])

kierrosData_temp <- kierrosData[,. (folder_path = tiedostot, SCRIPT_NM = strsplit(x=tiedostot, split = "/")[[1]][[2]])]
kierrosData_temp[, TABLE_NM := str_sub(SCRIPT_NM, 1 , -3)]

loop_result <- cbind(kierrosData_temp, palottele)

total_result_data <- rbind(total_result_data, loop_result)
}
}
ADM_DI_HIERARKIA <- total_result_data

