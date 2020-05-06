#' Variance of the Binomial distribution
#'
#' @description Variance of the Binomial distribution with size
#'  \eqn{n}{n} and probability of success \eqn{p}{p}.
#'
#' @templateVar q FALSE
#' @templateVar kappa FALSE
#' @template binom-template
#'
#' @examples
#'
#' V_binom(size = 3, prob = 0.5)
#'
#' @export
#'
V_binom <- function(size, prob) {
    size * prob * (1 - prob)
}
