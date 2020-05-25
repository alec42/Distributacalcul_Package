#' Tail Value-at-Risk d'une loi binomiale composée
#'
#' @param kap Niveau de confiance désiré
#' @param size n pour la binomiale
#' @param prob probabilité q pour la binomiale
#' @param shape alpha pour la Gamma
#' @param rate beta pour la Gamma
#' @param vark Value at Risk (VaR) calculé au même niveau de confiance kap
#' @param ko borne pour la somme dans le calcul de la TVaR
#' @param distr_severity choix de distribution de sévérité
#' @details Cette formule nécessite la formule de la VaR_bincomp (déjà installée avec le package tvarPackage). Cette formule utilise aussi un choix de (présentement) 2 distributions pour la sévérité; soit la Gamma ou la Lognormale.
#' @export
TVaR_BINCOMP <- function(kap, size, prob, shape, rate, vark, ko, distr_severity = "Gamma") {
    stopifnot(kap < 1, kap >= 0)

    if (vark == 0)
    {
        E_BINCOMP(size, prob, shape, rate, distr_severity) / (1 - kap)
    }
    else if (distr_severity == "Gamma")
    {
        (sum(sapply(1:ko, function(i) stats::dbinom(x = i, size = size, prob = prob) * (shape * i / rate) * stats::pgamma(q = vark, shape = shape * i + 1, rate = rate, lower.tail = FALSE))) / (1 - kap))
    }
}
