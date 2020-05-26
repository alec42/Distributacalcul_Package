#' Variance of the negative binomial distribution
#'
#' @description Variance of the negative binomial distribution with
#'  parameters \eqn{r} (number of successful trials) and \eqn{p}
#'  (probability of success).
#'
#' @template negbinom-template
#'
#' @export
#'
#' @examples
#'
#' # Where k is the number of trials for a rth success
#' V_negbinom(r = 2, p = .4)
#'
#' # Where k is the number of failures before a rth success
#' V_negbinom(r = 2, p = .4, nb_tries = TRUE)
#'
#' # With alternative parameterization where k is the number of trials
#' V_negbinom(r = 2, beta = 1.5)
#'
V_negbinom <- function(r, p = (1 / (1 + beta)), beta = ((1 - p) / p), nb_tries = F) {
    stopifnot(r > 0, p > 0, p < 1)

    r * ((1 - p) / (p^2))
}
