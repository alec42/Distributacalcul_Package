#' Fonction de répartition d'une loi poisson composée
#' @param k niveau de confiance désiré
#' @param lambda paramètre lambda pour la poisson
#' @param shape alpha pour la Gamma et mu pour la lognormale
#' @param rate beta pour la Gamma et sigma^2 pour la lognormale
#' @param ko borne pour l'optimisation de la VaR
#' @param distr_severity choix de distribution de sévérité. Gamma (par défaut) ou Lognormale
#' @details Cette formule utilise aussi un choix de 2 distributions pour la sévérité; soit la Gamma ou la Lognormale.
#' @export
VaR_PCOMP <- function(k, lambda, shape, rate, ko = 300, distr_severity = "Gamma")
{
    if(k <= p_PCOMP(x = 0, lambda = lambda, shape = shape, rate = rate, ko = ko))
        0
    else
        optimize(function(i) abs(p_PCOMP(x = i, lambda = lambda, shape = shape, rate = rate, ko = ko) - k), c(0, ko))$minimum
}
