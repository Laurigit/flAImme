custom_sample <- function(outcome_vect, prob_vect) {

  if (length(prob_vect) == 1) {
    return(outcome_vect)
  } else {
   res <- sample(outcome_vect, size = 1, prob = prob_vect)
   return(res)
  }

}



