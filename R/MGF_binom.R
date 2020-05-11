#' Moment Generating Function of the Binomial distribution
#'
#' @description Moment Generating Function (MGF) of the Binomial distribution
#'    with size \eqn{n}{n} and probability of success \eqn{p}{p}.
#'
#' @param t t
#' @template binom-template
#'
#' @examples
#'
#' MGF_binom(t = 1, size = 3, prob = 0.5)
#'
#' @export
#'
MGF_binom <- function(t, size, prob) {
    (prob * exp(t) + (1 - prob))^size
}
