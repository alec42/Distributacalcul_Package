#' Value-at-risk d'une loi binomiale composée
#' @param kappa Niveau de confiance désiré
#' @param n n pour la binomiale
#' @param q probabilité q pour la binomiale
#' @param shape alpha pour la Gamma
# et mu pour la lognormale
#' @param rate beta pour la Gamma
# et sigma^2 pour la lognormale
#' @param ko borne pour l'optimisation numérique de la VaR
#' @param distr_severity choix de distribution de sévérité. Gamma uniquement pour l'instant.
# @details Cette formule utilise aussi un choix de 2 distributions pour la sévérité; soit la Gamma (par défaut) ou la Lognormale.
#' @export
VaR_BINCOMP <- function(kappa, n, q, shape, rate, ko = 300, distr_severity = "Gamma")
{
    require(stats)
    if(kappa <= p_BINComp(0, n = n, q = q, ko = ko, shape = shape, rate = rate))
        0
    else
        optimize(function(i) abs(p_BINComp(i, n = n, q = q, ko = ko, shape = shape, rate = rate) - kappa), c(0, ko))$minimum
}
