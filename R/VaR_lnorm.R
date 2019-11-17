#' Value-at-Risk d'une loi lognormale
#' @param mu mu
#' @param sig sigma
#' @param kappa pourcentage de confiance désirée
#' @export
VaR_lnorm <- function(kappa, mu, sig)
{
    qlnorm(kappa, mu, sig)
}



