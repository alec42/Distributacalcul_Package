#' Tail Value-at-Risk  d'une loi Beta
#'
#' @param a alpha
#' @param b beta
#' @param kap kappa, doit être entre 0 et 1
#'
#' @export
TVaR_beta <- function(kap, a, b)
{
    fbeta <- a / (a+b)
    ftvar <- 1 / (1-kap)
    fbeta * ftvar * (pbeta(qbeta(p = kap, a, b, lower.tail = F), a+1, b))
}


