#' Variance d'une loi binomiale composée
#' @param n n pour la binomiale
#' @param q probabilité q pour la binomiale
#' @param shape alpha pour la Gamma et mu pour la lognormale
#' @param rate beta pour la Gamma et sigma^2 pour la lognormale
#' @param distr_severity choix de distribution de sévérité. Gamma (par défaut) ou Lognormale
#' @details Cette formule utilise aussi un choix de 2 distributions pour la sévérité; soit la Gamma ou la Lognormale.
#' @export
V_BINCOMP <- function(n, q, shape, rate, distr_severity = "Gamma")
{
    if(distr_severity == "Gamma")
    {
        (shape / rate)^2 * n * q * (1 - q) + n * q * V_gamma(a = shape, b = rate)
    }
    else if (distr_severity == "Lognormale")
        E_lnorm(mu = shape, sig = sqrt(rate))^2 * n * q * (1 - q) + n * q * V_lnorm(mu = shape, sig = rate)
}
