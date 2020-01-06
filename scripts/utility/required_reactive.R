#https://stackoverflow.com/questions/12982528/how-to-create-an-r-function-programmatically
#env <- parent.frame()
#reactiveName <- "test"
required_reactive <- function(default_value, reactiveName, env = parent.frame()) {
  if(!exists(as.character(substitute(reactiveName)), envir = env)) {
   as.function(alist(default_value), envir = env)
  } else {
    get(reactiveName)
  }
  
}


