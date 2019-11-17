#' Stop-Loss  d'une loi Beta
#' @param a alpha
#' @param b beta
#' @param d déductible
#' @export
SL_beta <- function(d, a, b)
{
  fact_beta <- a / (a+b)
  un <- fact_beta * (1 - pbeta(d, a+1, b))
  deux <- d * (1 - pbeta(d, a, b))
  un + deux
}


