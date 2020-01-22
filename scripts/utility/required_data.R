#required_data
#data_vector <- "SRC_PELIT"
#data_file<- "SRC_PELIT"

required_data <- function(data_vector, force_update = FALSE, saveR = FALSE, saveR_folder = "./temporary_files/",
                          input_env = globalenv(), rewriteSaveR = FALSE) {
  required_functions("find_and_source")
  #load("shiny_env.R")
  used_env <- input_env
  for(data_file in data_vector) {

    r_file_nm <- paste0(saveR_folder, data_file, ".RData")
    # lsdt <- data.table(object = ls(envir = used_env))
    # message(lsdt)
    # lsdt[, upper := toupper(object)]
    # lsdt[, match := upper == object]
    # message
    # message(lsdt[match == TRUE, object])

    if(!exists(data_file, envir = used_env ) | force_update == TRUE) {

      #check if the file exists as R object

      R_exists <- file.exists(r_file_nm)
      if(R_exists == FALSE | rewriteSaveR == TRUE) {
        list_of_object <- c(ls(envir = used_env), data_file)

        find_and_source(data_file, used_env)
        if(saveR == TRUE | (rewriteSaveR == TRUE & R_exists == TRUE)) {
          save(list = data_file,
               file = r_file_nm,
               compress = TRUE)
        }
        list_after_sourcing <- ls(envir = used_env)
        newly_created <- setdiff(list_after_sourcing,
                                 list_of_object)
        funlist <- lsf.str(envir = used_env)
        delete_list <- data.table(delete = setdiff(newly_created, funlist))
        delete_list[, upper := toupper(delete)]
        delete_list[, match := upper == delete]

        delete_list[match == FALSE, delete]

        rm(list = delete_list[match == FALSE, delete], envir = used_env)
      } else {
        load(r_file_nm, envir = used_env)
      }
    }else {
      # message("")
    }


  }
}
