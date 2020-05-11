#' Value-at-Risk d'une loi poisson composée
#'
#' @param kap niveau de confiance désiré
#' @param lambda paramètre lambda pour la poisson
#' @param shape alpha pour la Gamma
#' @param rate beta pour la Gamma
#' @param ko borne pour l'optimisation de la VaR
#' @param distr_severity choix de distribution de sévérité.
#' @export
VaR_PCOMP <- function(kap, lambda, shape, rate, distr_severity = "Gamma", ko)
{
    stopifnot(kap >= 0, kap <= 1, rate > 0)
    stopifnot(grepl(pattern = "(^Gamma$)", x = distr_severity, ignore.case = TRUE))

    if(kap <= p_PCOMP(x = 0, lambda = lambda, shape = shape, rate = rate, ko = ko))
        0
    else
        stats::optimize(function(i) abs(p_PCOMP(x = i, lambda = lambda, shape = shape, rate = rate, ko = ko) - kap), c(0, ko))$minimum
}
