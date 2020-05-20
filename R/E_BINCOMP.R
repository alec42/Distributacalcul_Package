#' Espérance d'une loi binomiale composée
#' @param n n pour la binomiale
#' @param q probabilité q pour la binomiale
#' @param shape alpha pour la Gamma et mu pour la lognormale
#' @param rate beta pour la Gamma et sigma^2 pour la lognormale
#' @param distr_severity choix de distribution de sévérité. Gamma (par défaut) ou Lognormale
#' @details Cette formule utilise aussi un choix de 2 distributions pour la sévérité; soit la Gamma ou la Lognormale.
#' @export
#'
#' @examples
#' E_BINCOMP(n = 1, q = 0.2, shape = log(1000) - 0.405, rate = 0.9^2,
#'           distr_severity = "Lognormale")
#'
E_BINCOMP <- function(n, q, shape, rate, distr_severity = "Gamma")
{
    if(distr_severity == "Gamma")
    {
        shape / rate * n * q
    }
    else if (distr_severity == "Lognormale")
        n * q * E_lnorm(shape, sqrt(rate))
}
