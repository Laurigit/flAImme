#required_data("ADM_DI_HIERARKIA")
#input_TABLE_NM_vector <- "SRC_DIVARI"
updateData <- function(input_TABLE_NM_vector, ADM_DI_HIERARKIA, input_env, rewriteSaveR = FALSE) {

  
  updateDataList <- function(input_TABLE_NM_vector_inner, ADM_DI_HIERARKIA, cumulative_list = NULL) {
    
    total_list <- cumulative_list
    for(input_TABLE_NM_inner in  input_TABLE_NM_vector_inner) {
      update_list <- ADM_DI_HIERARKIA[PARENT_TABLE_NM == input_TABLE_NM_inner, TABLE_NM]
      cumulative_list <- c(cumulative_list, update_list)
      total_list <-  unique(updateDataList(update_list, ADM_DI_HIERARKIA, cumulative_list))
      
    }
    
    return(total_list)  
  } 

  for(input_TABLE_NM in input_TABLE_NM_vector) {
    print(input_TABLE_NM)
    required_data(input_TABLE_NM, TRUE, input_env = input_env, rewriteSaveR = rewriteSaveR)
  updateData_list <- updateDataList(input_TABLE_NM, ADM_DI_HIERARKIA)  
  
    
    for (update_TABLE_NM in updateData_list) {
      print(update_TABLE_NM)
    required_data(update_TABLE_NM, TRUE, input_env = input_env,  rewriteSaveR = rewriteSaveR)
    }
  }
}

