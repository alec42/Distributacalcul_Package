#' Fonction de répartition d'une loi poisson composée
#' @param x x
#' @param lambda paramètre lambda pour la poisson
#' @param shape alpha pour la Gamma et mu pour la lognormale
#' @param rate beta pour la Gamma et sigma^2 pour la lognormale
#' @param ko borne pour la sommation de la fonction de répartition
#' @param distr_severity choix de distribution de sévérité. Gamma (par défaut) ou Lognormale
#' @details Cette formule utilise aussi un choix de 2 distributions pour la sévérité; soit la Gamma ou la Lognormale.
#' @export
p_PCOMP <- function(x, lambda, shape, rate, ko = 300, distr_severity = "Gamma"){
    if(distr_severity == "Gamma")
    {
        (dpois(x = 0, lambda) + sum(sapply(1:ko, function(k) dpois(x = k, lambda) * pgamma(q = x, shape * k, rate))))
    }
    else if (distr_severity == "Lognormale")
    {
        dpois(x = 0, lambda) + sum(sapply(1:ko, function(k) dpois(x = k, lambda) * plnorm(q = x, meanlog = shape * k, sdlog = sqrt(rate))))
    }
}
