#' Fonction de répartition d'une loi poisson composée
#' @param x x
#' @param lambda paramètre lambda pour la poisson
#' @param shape alpha pour la Gamma
#' @param rate beta pour la Gamma
#' @param ko borne pour la sommation de la fonction de répartition
#' @param distr_severity choix de distribution de sévérité.
#' @export
p_PCOMP <- function(x, lambda, shape, rate, ko = 300, distr_severity = "Gamma"){

    require(stats)
    if(distr_severity == "Gamma")
    {
        (dpois(x = 0, lambda) + sum(sapply(1:ko, function(k) dpois(x = k, lambda) * pgamma(q = x, shape * k, rate))))
    }
    # pas bon
    # else if (distr_severity == "Lognormale")
    # {
    #     dpois(x = 0, lambda) + sum(sapply(1:ko, function(k) dpois(x = k, lambda) * plnorm(q = x, meanlog = shape * k, sdlog = sqrt(rate))))
    # }
}
