rc <- function(tiedostonimi) {
  # tulos <- as.data.table(read_excel(path = paste0("./external_files/", tiedostonimi),
  #                    col_types = "text")
  # )
  tulos <- as.data.table(read.csv(paste0("../common_data/", tiedostonimi),
                                  sep = ";",
                                  stringsAsFactors = FALSE,
                                  dec = ",",
                                  colClasses = "character",
                                  fileEncoding = "UTF-8-BOM"))
  return(tulos)
}
