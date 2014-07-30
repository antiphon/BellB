#' Evaluate partial and complete Bell polynomial
#' 
#' Evaluates the partial and complete Bell polynomial, depending on parameters.
#' 
#' 
#' @param n number of elements
#' @param k number of elements to take (see details)
#' @param x vector of length n-k+1.
#' 
#' 
#' If k is missing, computes the complete Bell polynomial using summation over k=1:n
#' 
#' @references Cvijovic, D (2011). New identities for the partial Bell polynomials. DOI: 10.1016/j.aml.2011.03.043
#' 
#' @export

BellB <- function(n, k, x){
  if(n == 0) return(rep(1, ifelse(missing(k), 1, length(k))))
  if(missing(k)){ # complete
    k <- 1:n
    v <- sapply(k, BellB, n=n, x=x)
    sum(v)
  }
  else{ # partial
  #' recursion function
    rf <- function(m, l) {
      if(l < 1) {
        x[m]
      } 
      else{
        i <- l:(m-1)
        sum( choose(m, i) * x[m-i] * sapply(i, rf, l-1))
      }
    }
    K <- k-1
    v <- rf(n, K)
    
    v/factorial(k)
  }
}