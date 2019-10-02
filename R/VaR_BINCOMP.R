#' Value-at-risk d'une loi binomiale composée
#' @param kappa Niveau de confiance désiré
#' @param n n pour la binomiale
#' @param q probabilité q pour la binomiale
#' @param shape alpha pour la Gamma
#' @param rate beta pour la Gamma
#' @param ko borne pour l'optimisation numérique de la VaR
#' @param distr_severity choix de distribution de sévérité. Gamma uniquement pour l'instant.
#' @export
VaR_BINCOMP <- function(kappa, n, q, shape, rate, ko = 300, distr_severity = "Gamma")
{
    require(stats)
    if(kappa <= p_BINCOMP(0, n = n, q = q, ko = ko, shape = shape, rate = rate))
        0
    else
        optimize(function(i) abs(p_BINCOMP(i, n = n, q = q, ko = ko, shape = shape, rate = rate) - kappa), c(0, ko))$minimum
}
