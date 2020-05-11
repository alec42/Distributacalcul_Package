#' Value-at-Risk of the Binomial distribution
#'
#' @description Value-at-Risk of the Binomial distribution with size
#'  \eqn{n}{n} and probability of success \eqn{p}{p}. Wrapper of qbinom.
#'
#' @templateVar kap TRUE
#' @template binom-template
#'
#' @examples
#'
#' VaR_binom(kap = 0.8, size = 5, prob = 0.2)
#'
#' @export
#'
VaR_binom <- function(kap, size, prob) {
    stats::qbinom(p = kap, size, prob)
}
