#' Moment and Probability Generating Functions of the Binomial distribution
#'
#' @description Moment Generating Function (MGF) and Probability Generating
#'    Function (PGF) of the Binomial distribution with size \eqn{n}{n} and
#'    probability of success \eqn{p}{p}.
#'
#' @param t t
#' @templateVar q FALSE
#' @templateVar kappa FALSE
#' @template binom-template
#'
#' @examples
#'
#' MGF_binom(t = 1, size = 3, prob = 0.5)
#' PGF_binom(t = 1, size = 3, prob = 0.5)
#'
#' @export
#'
MGF_binom <- function(t, size, prob) {
    (prob * exp(t) + (1 - prob))^size
}

#' @rdname PGF_binom
PGF_binom <- function(t, size, prob) {
    (prob * t + (1 - prob))^size
}
