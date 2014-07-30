#' Bell numbers
#' 
#' Compute the nth Bell number.
#' 
#' @param n order
#' 
#' 
#' @export

BellN <- function(n){
  sapply(n, function(v) sum( sapply(1:v, stirling2, n=v ) ))
}
