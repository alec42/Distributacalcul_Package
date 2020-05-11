#' Fonction de répartition d'une loi poisson composée
#' @param x x
#' @param lambda paramètre lambda pour la poisson
#' @param shape alpha pour la Gamma
#' @param rate beta pour la Gamma
#' @param ko borne pour la sommation de la fonction de répartition
#' @param distr_severity choix de distribution de sévérité.
#' @export
#' @importFrom stats dpois
p_PCOMP <- function(x, lambda, shape, rate, ko = 300, distr_severity = "Gamma"){

    if(distr_severity == "Gamma")
    {
        (stats::dpois(x = 0, lambda) + sum(sapply(1:ko, function(k) stats::dpois(x = k, lambda) * stats::pgamma(q = x, shape * k, rate))))
    }
}
