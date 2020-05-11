#' Value-at-Risk of the compound Binomiale distribution
#'
#' @param kap Niveau de confiance désiré
#' @param n n pour la binomiale
#' @param q probabilité q pour la binomiale
#' @param shape alpha pour la Gamma
#' @param rate beta pour la Gamma
#' @param ko borne pour l'optimisation numérique de la VaR
#' @param distr_severity choix de distribution de sévérité. Gamma uniquement pour l'instant.
#'
#' @export
VaR_BINCOMP <- function(kap, n, q, shape, rate, distr_severity = "Gamma", ko)
{
    stopifnot(kap <= 1, kap >= 0, rate > 0)
    stopifnot(grepl(pattern = "(^Gamma$)", x = distr_severity, ignore.case = TRUE))

    if(kap <= p_BINCOMP(0, n = n, q = q, ko = ko, shape = shape, rate = rate))
        0
    else
        stats::optimize(function(i) abs(p_BINCOMP(i, n = n, q = q, ko = ko, shape = shape, rate = rate) - kap), c(0, ko))$minimum
}
