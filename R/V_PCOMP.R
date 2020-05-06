#' Variance d'une loi poisson composée
#' @param lambda paramètre lambda pour la poisson
#' @param shape alpha pour la Gamma et mu pour la lognormale
#' @param rate beta pour la Gamma et sigma^2 pour la lognormale
#' @param distr_severity choix de distribution de sévérité. Gamma (par défaut) ou Lognormale
#' @details Cette formule utilise aussi un choix de 2 distributions pour la sévérité; soit la Gamma ou la Lognormale.
#' @export
V_PCOMP <- function(lambda, shape, rate, distr_severity = "Gamma"){
    if(distr_severity == "Gamma"){
        lambda * kthmoment_gamma(k = 2, shape, rate)
    }
    else if (distr_severity == "Lognormale"){
        lambda * kthmoment_lnorm(k = 2, shape, sqrt(rate))
    }
}
