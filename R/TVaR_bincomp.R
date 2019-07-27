#' Tail Value-at-risk d'une loi Binomial Composéee
#' @param k Niveau de confiance désiré
#' @param vark Value at Risk (VaR) calculé au même niveau de confiance k
#' @param n n pour la binomiale
#' @param q probabilité q pour la binomiale
#' @param shape alpha pour la Gamma et mu pour la lognormale
#' @param rate beta pour la Gamma et sigma^2 pour la lognormale
#' @param ko borne pour la somme dans le calcul de la TVaR
#' @param distr_severity choix de distribution de sévérité. Gamma ou Lognormale
#' @details Cette formule nécessite la formule de la VaR_bincomp (déjà installée avec le package tvarPackage). Cette formule utilise aussi un choix de (présentement) 2 distributions pour la sévérité; soit la Gamma ou la Lognormale.
#' @export
TVaR_BINComp <- function(k, shape, rate, n, q, vark, ko, distr_severity = "Gamma"){

    if (vark == 0)
    {
        E_BINComp(n = n, q = q, shape = shape, rate = rate, distr_severity = distr_severity) / (1 - k)
    }
    else if (distr_severity == "Gamma")
    {
        (sum(sapply(1:ko, function(i) dbinom(x = i, size = n, prob = q) * (shape * i / rate) * pgamma(q = vark, shape = shape * i + 1, rate = rate, lower.tail = F))) / (1 - k))
    }
    else if (distr_severity == "Lognormale")
    {
        (sum(sapply(1:ko, function(i) dbinom(x = i, size = n, prob = q) * (shape * i / rate) * plnorm(q = vark, meanlog = shape * i + 1, sdlog = sqrt(rate), lower.tail = F))) / (1 - k))
    }
}
