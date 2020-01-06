#required_data("ADM_DI_HIERARKIA")
#input_TABLE_NM_vector <- "ADM_AVAILABLE_BETS_OPEN"
#input_TABLE_NM <- "ADM_AVAILABLE_BETS_OPEN"
#input_TABLE_NM_vector_inner <- "ADM_AVAILABLE_BETS_OPEN"

revUpdate <- function(input_TABLE_NM_vector) {
   input_env <- globalenv()
   rewriteSaveR <- FALSE

  revUpdateDataList <- function(input_TABLE_NM_vector_inner, cumulative_list = NULL) {
    required_data("ADM_DI_HIERARKIA")
    total_list <- cumulative_list
    for (input_TABLE_NM_inner in  input_TABLE_NM_vector_inner) {
      update_list <- ADM_DI_HIERARKIA[TABLE_NM == input_TABLE_NM_inner, PARENT_TABLE_NM]

      cumulative_list <- c(update_list, cumulative_list )
      total_list <-  unique(revUpdateDataList(update_list, cumulative_list))
     # print(total_list)
    }

    return(total_list)
  }

  for(input_TABLE_NM in input_TABLE_NM_vector) {


    updateData_list <- revUpdateDataList(input_TABLE_NM)


    for (update_TABLE_NM in updateData_list) {
     # print(update_TABLE_NM)
      required_data(update_TABLE_NM, TRUE, input_env = input_env,  rewriteSaveR = rewriteSaveR)
    }
   # print(input_TABLE_NM)
    required_data(input_TABLE_NM, TRUE, input_env = input_env, rewriteSaveR = rewriteSaveR)
  }
}

