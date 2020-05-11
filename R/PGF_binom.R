#' Probability Generating Function of the Binomial distribution
#'
#' @description Probability Generating Function (PGF) of the Binomial distribution
#'    with size \eqn{n}{n} and probability of success \eqn{p}{p}.
#'
#' @param t t
#' @template binom-template
#'
#' @examples
#'
#' PGF_binom(t = 1, size = 3, prob = 0.5)
#'
#' @export
#'
PGF_binom <- function(t, size, prob) {
    (prob * t + (1 - prob))^size
}
