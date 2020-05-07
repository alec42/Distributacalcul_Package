#' Value-at-risk d'une loi binomiale négative composée
#' @param kap Niveau de confiance désiré
#' @param r r pour la binomiale négative (nombre d'échecs).
#' @param q probabilité q pour la binomiale
#' @param shape alpha pour la Gamma
#' @param rate beta pour la Gamma
#' @param ko borne pour l'optimisation numérique de la VaR
#' @param distr_severity choix de distribution de sévérité.
#' @export
VaR_BNCOMP <- function(kap, r, q, shape, rate, ko = 300, distr_severity = "Gamma")
{
    require(stats)
    if(kap <= p_BNCOMP(0, r, q, shape, rate, ko, distr_severity))
        0
    else
        optimize(function(i) abs(p_BNCOMP(i, r, q, ko, shape, rate) - kap), c(0, ko))$minimum
}
