#' Value-at-Risk  d'une loi Beta
#' @param a alpha
#' @param b beta
#' @param k kappa (doit être entre 0 et 1)
#' @export
VaR_beta <- function(k, a, b)
{
    qbeta(k, a, b)
}


