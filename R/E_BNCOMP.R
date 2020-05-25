#' Expected value of the compound Negative Binomial distribution
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
#' E_BNCOMP(size = 4, prob = 0.2, shape = 0, rate = 1, distr_severity = "Lognormal")
#' E_BNCOMP(size = 4, prob = 0.4, shape = 0, rate = 2, distr_severity = "lognormal")
#'
#' @export
#'
E_BNCOMP <- function(size, prob, shape, rate, distr_severity = "Gamma")
{
    stopifnot(prob >= 0, prob <= 1, rate > 0)
    stopifnot(grepl(pattern = "(^Gamma$)|(^Lognormal[e]*$)", x = distr_severity, ignore.case = TRUE))

    if (grepl(pattern = "^Gamma$", x = distr_severity, ignore.case = TRUE)) {
        if (missing(rate)) { # checks if user specified rate
            rate = 1 / scale
        }
        stopifnot(shape > 0, rate | scale > 0)
        E.NBCOMP <- E_negbinom(size, prob, nb_tries = FALSE) * kthmoment_gamma(k = 1, shape, rate)
    } else if (grepl(pattern = "^Lognormal[e]*$", x = distr_severity, ignore.case = TRUE)) {
        E.NBCOMP <- E_negbinom(size, prob, nb_tries = FALSE) * E_lnorm(shape, sqrt(rate))
    } else {
        stop("Please enter a valid distribution choice. Either 'Lognormal' or 'Gamma'")
    }

    return(E.NBCOMP)
}
