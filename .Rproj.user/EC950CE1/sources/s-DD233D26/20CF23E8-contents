#' Tail Value-at-Risk  d'une loi Beta
#' @param a alpha
#' @param b beta
#' @param k kappa (doit être entre 0 et 1)
#' @export
TVaR_beta <- function(k, a, b)
{
    fbeta <- a / (a+b)
    ftvar <- 1 / (1-k)
    fbeta * ftvar * (1 - pbeta(VaR_beta(k, a, b), a+1, b))
}


