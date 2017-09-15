#' Find greatest common divisor between two numbers
#' 
#' @param a A number.
#' @param b A number.
#' @return Greatest common division using Euclidean's algorithm
#' @examples
#' euclidean(1000, 100)
#' euclidean(10839893, 13238)
#' @export euclidean

euclidean <-
function(a,b)
{ stopifnot(is.numeric (a) && is.numeric (b))
  a0=a # first element of sequence
  a1=b # second element
  while (a1!=0) 
  {
    r=a0%%a1 # rest of euclidean division of a0 by a1
    a0=a1 
    a1=r
  }
  return(a0)
}
