#' @title Test for prime number
#' @author Thomas Bryce Kelly
#' @keywords prime
isPrime = function(x) {
  x = round(abs(x))
  if (x == 2 | x == 3) {
    return(T)
  }
  for (i in c(2, seq(3, floor(sqrt(x)), by = 2))) {
    if (x %% i == 0) {
      return(F)
    }
  }
  return(T)
}




