#create SRC and STG files
create_files <- function(input_name, ADM = FALSE) {
  #first create dirs
  onko_SRC <- setdiff(c("SRC", "ADM", "utility", "solution", "STG"),dir("./scripts/"))
  lapply(onko_SRC, function(x) {dir.create(paste0("./scripts/", x))})
  #then files



 if (!file.exists(paste0("./scripts/SRC/SRC_", input_name, ".R"))) {
   file.create(paste0("./scripts/SRC/SRC_", input_name, ".R" ))
     src_text <- paste0("#SRC_", input_name, "\n",
                        "\n",
                        'luettu <- dbSelectAll("', input_name, '", con) \n',
                        "\n",
                        "SRC_", input_name, " <- fix_colnames(luettu)"
     )
  write(src_text, paste0("./scripts/SRC/SRC_", input_name, ".R"))

 }
 #STG
  if( (!file.exists(paste0("./scripts/STG/STG_", input_name, ".R")))) {
    file.create(paste0("./scripts/STG/STG_", input_name, ".R" ))
    stg_text <- paste0('#STG_', input_name, '\n',
                       '\n',
                       'required_data(c("SRC_', input_name, '"))', '\n',
                       '\n',
                       'STG_', input_name, ' <- SRC_', input_name)
    write(stg_text, paste0("./scripts/STG/STG_", input_name, ".R"))
  }

  #ADM
  if (ADM == TRUE) {
    if( (!file.exists(paste0("./scripts/ADM/ADM_", input_name, ".R")))) {
      file.create(paste0("./scripts/ADM/ADM_", input_name, ".R" ))
      adm_text <- paste0('#ADM_', input_name, '\n',
                         '\n',
                         'required_data(c("STG_', input_name, '"))', '\n',
                         '\n',
                         'ADM_', input_name, ' <- STG_', input_name)
      write(adm_text, paste0("./scripts/ADM/ADM_", input_name, ".R"))
    }
    file.edit(paste0("./scripts/ADM/ADM_", input_name, ".R" ))
  }

  file.edit(paste0("./scripts/STG/STG_", input_name, ".R" ))
  file.edit(paste0("./scripts/SRC/SRC_", input_name, ".R" ))
  print(data.table(dir("./source_data/")))
}
