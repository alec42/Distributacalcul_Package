#' Tail Value-at-risk d'une loi poisson composée
#' @param kappa Niveau de confiance désiré
#' @param vark Value at Risk (VaR) calculé au même niveau de confiance kappa
#' @param lambda lambda
#' @param shape alpha pour la Gamma
#' @param rate beta pour la Gamma
#' @param ko borne pour la somme dans le calcul de la TVaR
#' @param distr_severity choix de distribution de sévérité.
#' @details Cette formule nécessite la formule de la VaR_pcomp (déjà installée avec le package tvarPackage). Cette formule utilise aussi un choix de (présentement) 2 distributions pour la sévérité; soit la Gamma ou la Lognormale.
#' @export
TVaR_PCOMP <- function(kappa, lambda, shape, rate, vark, ko = 300, distr_severity = "Gamma")
{
    require(stats)
    if (vark == 0)
    {
        E_PCOMP(rate, shape, lambda, distr_severity) / (1 - kappa)
    }
    else if (distr_severity == "Gamma")
    {
        sum(sapply(1:ko, function(k) dpois(x = k, lambda) * ( shape * k )/rate * pgamma(q = vark, shape = shape * k + 1, rate, lower.tail = F)))/(1 - kappa)
    }

}
