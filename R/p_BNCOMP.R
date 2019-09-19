#' Répartition d'une loi binomiale négative composée
#' @param x x
#' @param r r pour la binomiale négative (nombre d'échecs).
#' @param q probabilité q pour la binomiale
#' @param shape alpha pour la Gamma
#  et mu pour la lognormale
#' @param rate beta pour la Gamma
# et sigma^2 pour la lognormale
#' @param ko borne pour la sommation de la fonction de répartition
#' @param distr_severity choix de distribution de sévérité.
#  Gamma ou Lognormale
# @details Cette formule utilise aussi un choix de 2 distributions pour la sévérité; soit la Gamma (par défaut) ou la Lognormale.
#' @export
p_BNCOMP <- function(x, r, q, shape, rate, ko, distr_severity = "Gamma")
{
    require(stats)
    if(distr_severity == "Gamma")
    {
        dnbinom(x = 0, size = r, prob = q) + sum(sapply(1:ko, function(i) dnbinom(x = i, size = r, prob = q) * pgamma(q = x, shape = shape * i, rate = rate)))
    }
    # pas bon
    # else if (distr_severity == "Lognormale")
    # {
    #     # dnbinom(x = 0, size = r, prob = q) + sum(sapply(1:ko, function(i) dnbinom(x = i, size = r, prob = q) * plnorm(q = x, meanlog = shape * i, sdlog = sqrt(rate))))
    # }
}
