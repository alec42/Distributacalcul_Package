#' Tail Value-at-Risk d'une loi poisson composée
#'
#' @param kap Desired confidence level
#' @param vark Value at Risk (VaR) calculé au même niveau de confiance kap
#' @param lambda lambda rate paramter
#' @param shape alpha for Gamma
#' @param rate beta for Gamma
#' @param k0 point up to which to sum the distribution to approximate the expected value.
#' @param distr_severity choix de distribution de sévérité.
#' @details Cette formule nécessite la formule de la VaR_pcomp (déjà installée avec le package tvarPackage). Cette formule utilise aussi un choix de (présentement) 2 distributions pour la sévérité; soit la Gamma ou la Lognormale.
#' @export
#' @importFrom stats pgamma dpois
TVaR_PCOMP <- function(kap, lambda, shape, rate, vark, distr_severity = "Gamma", k0)
{
    stopifnot(kap < 1, kap >= 0)

    if (vark == 0)
    {
        E_PCOMP(rate, shape, lambda, distr_severity) / (1 - kap)
    }
    else if (distr_severity == "Gamma")
    {
        sum(sapply(1:k0, function(k) stats::dpois(x = k, lambda) * ( shape * k )/rate * stats::pgamma(q = vark, shape = shape * k + 1, rate, lower.tail = FALSE)))/(1 - kap)
    }

}
