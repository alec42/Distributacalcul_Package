#' Value-at-risk d'une loi binomiale négative composée
#' @param k Niveau de confiance désiré
#' @param r r pour la binomiale négative (nombre d'échecs).
#' @param q probabilité q pour la binomiale
#' @param shape alpha pour la Gamma et mu pour la lognormale
#' @param rate beta pour la Gamma et sigma^2 pour la lognormale
#' @param ko borne pour l'optimisation numérique de la VaR
#' @param distr_severity choix de distribution de sévérité. Gamma ou Lognormale
#' @details Cette formule utilise aussi un choix de 2 distributions pour la sévérité; soit la Gamma (par défaut) ou la Lognormale.
#' @export
VaR_BNCOMP <- function(k, r, q, shape, rate, ko = 300, distr_severity = "Gamma")
{
    require(stats)
    if(k <= p_BNCOMP(0, r, q, ko, shape, rate))
        0
    else
        optimize(function(i) abs(p_BNCOMP(i, r, q, ko, shape, rate) - k), c(0, ko))$minimum
}
