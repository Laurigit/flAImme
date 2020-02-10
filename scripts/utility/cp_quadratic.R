cp.quadratic <- function(p, n) {

  if (length(p) < n) {
    result <- 0
  } else {
  P <- matrix(0, nrow=length(p), ncol=length(p))
  P[1,] <- rev(cumsum(rev(p * prod(1-p) / (1-p))))
  for (i in seq(2, length(p))) {
    P[i,] <- c(rev(cumsum(rev(head(p, -1) / (1-head(p, -1)) * tail(P[i-1,], -1)))), 0)
  }
 # print(c(prod(1-p), P[,1]))
  #print(c(prod(1-p), P[,1])[(n + 1):(length(p) +1)])
  result <- sum(c(prod(1-p), P[,1])[(n + 1):(length(p) +1)])
  }
  return(result)
}
# p <- c(0.5, 0.4, 0.3)
# res <- cp.quadratic(p, 2)
# res
