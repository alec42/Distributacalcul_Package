#' Tail Value-at-risk d'une loi binomiale négative composée
#' @param kap Niveau de confiance désiré
#' @param r r pour la binomiale
#' @param q probabilité q pour la binomiale négative
#' @param shape alpha pour la Gamma
#' @param rate beta pour la Gamma
#' @param vark Value at Risk (VaR) calculé au même niveau de confiance kap
#' @param ko borne pour la somme dans le calcul de la TVaR.
#' @param distr_severity choix de distribution de sévérité.
#' @details Cette formule nécessite la formule de la VaR_bncomp (déjà installée avec le package tvarPackage). Cette formule utilise aussi un choix de (présentement) 2 distributions pour la sévérité; soit la Gamma ou la Lognormale.
#' @export
TVaR_BNCOMP <- function(kap, r, q, shape, rate, vark, ko, distr_severity = "Gamma"){

    require(stats)
    if (vark == 0)
    {
        E_BNComp(r = r, q = q, shape = shape, rate = rate, distr_severity = distr_severity) / (1 - kap)
    }
    else if (distr_severity == "Gamma")
    {
        (sum(sapply(1:ko, function(i) dnbinom(x = i, size = r, prob = q) * (shape / rate) * pgamma(q = vark, shape = shape * i + 1, rate = rate, lower.tail = F))) / (1 - kap))
    }
    else if (distr_severity == "Lognormale")
    {
        (sum(sapply(1:ko, function(i) dnbinom(x = i, size = r, prob = q) * (shape / rate) * plnorm(q = vark, meanlog = shape * i + 1, sdlog = sqrt(rate), lower.tail = F))) / (1 - kap))
    }
}
