#' Espérance d'une loi poisson composée
#' @param lambda paramètre lambda pour la poisson
#' @param shape alpha pour la Gamma et mu pour la lognormale
#' @param rate beta pour la Gamma et sigma^2 pour la lognormale
#' @param distr_severity choix de distribution de sévérité. Gamma (par défaut) ou Lognormale
#' @details Cette formule utilise aussi un choix de 2 distributions pour la sévérité; soit la Gamma ou la Lognormale.
#' @export
E_PCOMP <- function(lambda, shape, rate, distr_severity = "Gamma"){
    if(distr_severity == "Gamma"){
        shape / rate * lambda
    }
    else if (distr_severity == "Lognormale"){
        exp(shape + rate / 2) * lambda
    }
}
