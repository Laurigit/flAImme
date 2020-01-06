#fix_colnames
fix_colnames <- function(table_name) {
  colname_vect <- colnames(table_name)
  whitespace <- gsub(" ", "", colname_vect, fixed = TRUE)
  aakkoset_a  <- gsub("ä", "a", whitespace, fixed = TRUE)
  aakkoset_o  <- gsub("ö", "o", aakkoset_a, fixed = TRUE)
  colnames(table_name) <- aakkoset_o
  return(table_name)
}
