#' Répartition d'une loi binomiale négative composée
#' @param x x
#' @param size r pour la binomiale négative (nombre d'échecs).
#' @param prob probabilité q pour la binomiale
#' @param shape alpha pour la Gamma
#' @param rate beta pour la Gamma
#' @param ko borne pour la sommation de la fonction de répartition
#' @param distr_severity choix de distribution de sévérité.
#'
#' @examples
#' p_BNCOMP(x = 5E5, ko = 1E3, size = 5, prob = 1/5,
#'          shape = 2, rate = 1/5000, distr_severity = "Gamma")
#'
#' @export
#' @importFrom stats dnbinom
#'
p_BNCOMP <- function(x, size, prob, shape, rate, ko, distr_severity = "Gamma") {
    stopifnot(prob >= 0, prob <= 1, rate > 0)
    stopifnot(grepl(pattern = "(^Gamma$)", x = distr_severity, ignore.case = TRUE))

    if (grepl(pattern = "^Gamma$", x = distr_severity, ignore.case = TRUE)) {
        stats::dnbinom(x = 0, size = size, prob = prob) +
            sum(sapply(1:ko, function(i) stats::dnbinom(x = i, size = size, prob = prob) * stats::pgamma(q = x, shape = shape * i, rate = rate)))
    }
}
