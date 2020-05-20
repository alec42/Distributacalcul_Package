#' Moment Generating Function of the Negative Binomial distribution
#'
#' @description Moment Generating Function (PGF) of the Negative
#'    Binomial distribution with parameters \eqn{r} (number of
#'    successful trials) and \eqn{p} (probability of success).
#'
#' @param t t
#' @template negbinom-template
#'
#' @examples
#'
#' MGF_negbinom(t = 1, r = 4, p = 0.5)
#'
#' @export
#'
MGF_negbinom <- function(t, r, p = (1 / (1 + beta)), beta = ((1 - p) / p), nb_tries = F) {
    (
        p / (1 - (1 - p) * exp(t))
    )^r
}
