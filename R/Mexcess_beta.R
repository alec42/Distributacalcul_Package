#' Mean Excess-Loss  d'une loi Beta
#' @param a alpha
#' @param b beta
#' @param d déductible
#' @export
Mexcess_beta <- function(d, a, b)
{
    fbeta <- a / (a+b)
    num1 <- 1 - pbeta(d, a+1, b)
    denom1 <- 1 - pbeta(d, a, b)
    (fbeta * (num1 / denom1)) - d
}


