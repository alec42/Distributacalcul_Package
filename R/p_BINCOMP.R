#' Fonction de répartition d'une loi binomiale composée
#' @param x x
#' @param n n pour la binomiale
#' @param q probabilité q pour la binomiale
#' @param shape alpha pour la Gamma et mu pour la lognormale
#' @param rate beta pour la Gamma et sigma^2 pour la lognormale
#' @param ko borne pour la somme de la fonction de répartition
#' @param distr_severity choix de distribution de sévérité. Gamma ou Lognormale
#' @details Cette formule utilise aussi un choix de 2 distributions pour la sévérité; soit la Gamma ou la Lognormale.
#' @export
p_BINCOMP <- function(x, n, q, shape, rate, ko, distr_severity = "Gamma")
{
    require(stats)
    if(distr_severity == "Gamma")
    {
        dbinom(x = 0, size = n, prob = q) + sum(sapply(1:ko, function(i) dbinom(x = i, size = n, prob = q) * pgamma(q = x, shape = shape * i, rate = rate)))
    }
    else if (distr_severity == "Lognormale")
    {
        dbinom(x = 0, size = n, prob = q) + sum(sapply(1:ko, function(i) dbinom(x = i, size = n, prob = q) * plnorm(q = x, meanlog = shape * i, sdlog = sqrt(rate))))
    }
}
