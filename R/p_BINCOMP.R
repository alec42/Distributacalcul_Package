#' Fonction de répartition d'une loi binomiale composée
#' @param x x
#' @param size n pour la binomiale
#' @param prob probabilité q pour la binomiale
#' @param shape alpha pour la Gamma
#' @param rate beta pour la Gamma
#' @param ko borne pour la somme de la fonction de répartition
#' @param distr_severity choix de distribution de sévérité.
#' @export
#' @importFrom stats dbinom
p_BINCOMP <- function(x, size, prob, shape, rate, ko, distr_severity = "Gamma")
{
    if(distr_severity == "Gamma")
    {
        stats::dbinom(x = 0, size = size, prob = prob) + sum(sapply(1:ko, function(i) stats::dbinom(x = i, size = size, prob = prob) * stats::pgamma(q = x, shape = shape * i, rate = rate)))
    }
}
