#' Fonction de répartition d'une loi binomiale composée
#' @param x x
#' @param n n pour la binomiale
#' @param q probabilité q pour la binomiale
#' @param shape alpha pour la Gamma
#' @param rate beta pour la Gamma
#' @param ko borne pour la somme de la fonction de répartition
#' @param distr_severity choix de distribution de sévérité.
#' @export
#' @importFrom stats dbinom
p_BINCOMP <- function(x, n, q, shape, rate, ko, distr_severity = "Gamma")
{
    require(stats)
    if(distr_severity == "Gamma")
    {
        stats::dbinom(x = 0, size = n, prob = q) + sum(sapply(1:ko, function(i) stats::dbinom(x = i, size = n, prob = q) * stats::pgamma(q = x, shape = shape * i, rate = rate)))
    }
    # pas bon
    # else if (distr_severity == "Lognormale")
    # {
    #     dbinom(x = 0, size = n, prob = q) + sum(sapply(1:ko, function(i) dbinom(x = i, size = n, prob = q) * plnorm(q = x, meanlog = shape * i, sdlog = sqrt(rate))))
    # }
}
