dtim <- function(){
  vanha <- ifelse(!exists("vanha"), now(), vanha)
  res <-  as.numeric(now()) -vanha
  vanha <<- now()
  return(res)
}
