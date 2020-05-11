#' Value-at-Risk d'une loi binomiale négative composée
#'
#' @param kap Niveau de confiance désiré
#' @param size r pour la binomiale négative
#' @param prob probabilité q pour la binomiale
#' @param shape alpha pour la Gamma et mu pour la lognormale
#' @param rate beta pour la Gamma et sigma^2 pour la lognormale
#' @param distr_severity choice of severity distribution Gamma (default) or Lognormal
#' @param ko borne pour la somme dans le calcul de la TVaR.
#'
#' @examples
#'
#' VaR_BNCOMP(kap = 0.99, size = 4, prob = 0.4, shape = 5, rate = 10, distr_severity = "Gamma", ko = 1500)
#'
#' @export
#'
VaR_BNCOMP <- function(kap, size, prob, shape, rate, distr_severity = "Gamma", ko) {
    stopifnot(prob >= 0, prob <= 1, rate > 0)
    stopifnot(grepl(pattern = "(^Gamma$)", x = distr_severity, ignore.case = TRUE))

    if (kap <= p_BNCOMP(x = 0, size, prob, shape, rate, ko = ko, distr_severity = distr_severity)) {
        0
    } else {
        optimize(function(i) abs(p_BNCOMP(i, size, prob, ko, shape, rate) - kap), c(0, ko))$minimum
    }
}
