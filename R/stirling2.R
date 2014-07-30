#' Stirling numbers
#' 
#' @export

stirling2 <- function(n, k) {
  sapply(k, function(K) {
    i <- 0:K
    sum( (-1)^i*choose(K,i)*(K-i)^n  )/factorial(K)
  })
}