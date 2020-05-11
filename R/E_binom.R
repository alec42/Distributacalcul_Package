#' Expected value of the Binomial distribution
#'
#' @description Expected value of the Binomial distribution with size
#'  \eqn{n}{n} and probability of success \eqn{p}{p}.
#'
#' @template binom-template
#'
#' @examples
#'
#' E_binom(size = 3, prob = 0.5)
#'
#' @export
#'
E_binom <- function(size, prob) {
    size * prob
}
