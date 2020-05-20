#' Tail Value-at-Risk d'une loi binomiale composée
#'
#' @param kap Niveau de confiance désiré
#' @param n n pour la binomiale
#' @param q probabilité q pour la binomiale
#' @param shape alpha pour la Gamma
#' @param rate beta pour la Gamma
#' @param vark Value at Risk (VaR) calculé au même niveau de confiance kap
#' @param ko borne pour la somme dans le calcul de la TVaR
#' @param distr_severity choix de distribution de sévérité
#' @details Cette formule nécessite la formule de la VaR_bincomp (déjà installée avec le package tvarPackage). Cette formule utilise aussi un choix de (présentement) 2 distributions pour la sévérité; soit la Gamma ou la Lognormale.
#' @export
TVaR_BINCOMP <- function(kap, n, q, shape, rate, vark, ko, distr_severity = "Gamma"){
    stopifnot(kap < 1, kap >= 0)

    require(stats)
    if (vark == 0)
    {
        E_BINCOMP(n = n, q = q, shape = shape, rate = rate, distr_severity = distr_severity) / (1 - kap)
    }
    else if (distr_severity == "Gamma")
    {
        (sum(sapply(1:ko, function(i) stats::dbinom(x = i, size = n, prob = q) * (shape * i / rate) * stats::pgamma(q = vark, shape = shape * i + 1, rate = rate, lower.tail = F))) / (1 - kap))
    }
}
