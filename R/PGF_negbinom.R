#' Probability Generating Function of the Negative Binomial distribution
#'
#' @description Probability Generating Function (PGF) of the Negative
#'    Binomial distribution with parameters \eqn{r} (number of
#'    successful trials) and \eqn{p} (probability of success).
#'
#' @param t t
#' @template negbinom-template
#'
#' @examples
#'
#' PGF_negbinom(t = 5, r = 3, p = 0.3)
#'
#' @export
#'
PGF_negbinom <- function(t, r, p = (1 / (1 + beta)), beta = ((1 - p) / p), nb_tries = F) {

    stopifnot(r > 0, p > 0, p < 1)
    (
        p / (1 - (1 - p) * t)
    )^r
}
