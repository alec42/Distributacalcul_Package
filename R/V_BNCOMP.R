#' Variance d'une loi binomiale négative composée
#' @param r r pour la binomiale négative
#' @param q probabilité q pour la binomiale
#' @param shape alpha pour la Gamma et mu pour la lognormale
#' @param rate beta pour la Gamma et sigma^2 pour la lognormale
#' @param distr_severity choix de distribution de sévérité. Gamma (par défaut) ou Lognormale
#' @details Cette formule utilise aussi un choix de 2 distributions pour la sévérité; soit la Gamma ou la Lognormale.
#' @export
V_BNCOMP <- function(r, q, shape, rate, distr_severity = "Gamma")
{
    if(distr_severity == "Gamma")
    {
        (shape / rate)^2 * (r * (1 - q)/(q^2)) + (shape / rate^2) * (r * (1 - q) / q)
    }
    else if (distr_severity == "Lognormale")
        (r * (1 - q)/q)*(E_lnorm(shape, sqrt(rate))/q + V_lnorm(shape, sqrt(rate)))
}
