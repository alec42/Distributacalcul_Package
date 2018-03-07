#' Tail Value-at-Risk d'une loi lognormale
#' @param mu mu
#' @param sig sigma
#' @param kappa pourcentage de confiance désiré
#' @export
TVaR_lnorm <- function(kappa, mu, sig)
{
    fact_tvar <- 1 / (1-kappa)
    esp <- exp(mu  +(sig^2)/2)
    phi <- qnorm(kappa) - sig
    fact_tvar * esp * (1- phi)
}



