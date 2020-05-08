#' Variance d'une loi binomiale négative composée
#'
#' @details Cette formule utilise aussi un choix de 2 distributions pour la sévérité; soit la Gamma ou la Lognormale.
#'
#' @param size r pour la binomiale négative
#' @param prob probabilité q pour la binomiale
#' @param shape alpha pour la Gamma et mu pour la lognormale
#' @param rate beta pour la Gamma et sigma^2 pour la lognormale
#' @param distr_severity choice of severity distribution Gamma (default) or Lognormal
#'
#' @examples
#'
#' V_BNCOMP(size = 5, prob = 1/5, shape = 2, rate = 1/5000, distr_severity = "Gamma")
#'
#' @export
#'
V_BNCOMP <- function(size, prob, shape, rate, distr_severity = "Gamma") {
    stopifnot(prob >= 0, prob <= 1, rate > 0)
    stopifnot(grepl(pattern = "(^Gamma$)|(^Lognormal[e]*$)", x = distr_severity, ignore.case = TRUE))

    if (grepl(pattern = "^Gamma$", x = distr_severity, ignore.case = TRUE)) {
        V.BNCOMP <- E_gamma(shape, rate)^2 * V_negbinom(size, prob, nb_tries = FALSE) + V_gamma(shape, rate) * E_negbinom(size, prob, nb_tries = FALSE)
    } else if (grepl(pattern = "^Lognormal[e]*$", x = distr_severity, ignore.case = TRUE)) {
        V.BNCOMP <- E_negbinom(size, prob, nb_tries = FALSE) * (E_lnorm(shape, sqrt(rate)) / prob + V_lnorm(shape, sqrt(rate)))
    } else {
        stop("Please enter a valid distribution choice. Either 'Lognormal' or 'Gamma'")
    }

    return(V.BNCOMP)
}
