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
#' PGF_negbinom(t = 5, size = 3, prob = 0.3)
#'
#' @export
#'
PGF_negbinom <- function(t, size, prob = (1 / (1 + beta)), beta = ((1 - prob) / prob), nb_tries = F) {

    stopifnot(size > 0, prob > 0, prob < 1)
    (
        prob / (1 - (1 - prob) * t)
    )^size
}
