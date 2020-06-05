#' Moment Generating Function of the Negative Binomial distribution
#'
#' @description Moment Generating Function (PGF) of the Negative
#'    Binomial distribution with parameters \eqn{r} (number of
#'    successful trials) and \eqn{p} (probability of success).
#'
#' @param t t
#' @template negbinom-template
#'
#' @export
#'
#' @examples
#'
#' MGF_negbinom(t = 1, size = 4, prob = 0.5)
#'
MGF_negbinom <- function(t, size, prob = (1 / (1 + beta)), beta = ((1 - prob) / prob), nb_tries = FALSE) {

    stopifnot(size > 0, prob > 0, prob < 1)
    (
        prob / (1 - (1 - prob) * exp(t))
    )^size
}
