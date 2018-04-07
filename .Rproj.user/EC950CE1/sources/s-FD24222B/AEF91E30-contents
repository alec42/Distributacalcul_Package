#' Tail Value-at-Risk d'une loi normale
#' @param mu mu
#' @param sig sigma
#' @param kappa pourcentage de confiance désiré
#' @export
TVaR_norm <- function(kappa, mu, sig)
{
    fact_tvar <- 1 / (1-kappa)
    exposant <- -(qnorm(kappa, mu, sig))^2 / 2
    mu + fact_tvar * sig * (1/sqrt(2*pi)) * exp(exposant)
}


