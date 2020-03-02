cost_function_for_optimization <- function(i, j, k, length_k,  finish_line) {

  costdata <- data.table(i, j, k, length_k)

  # costdata[, cost := 1 + as.numeric(k == length_k * ((100 + 10 ) - j))]
  #length_k means that use extra exhaust after deck is finished
  costdata[, cost := pmax(finish_line - i - 2, 0) ^ 3 * (k == length_k) + 0.1]
  return(costdata[, cost])

}
