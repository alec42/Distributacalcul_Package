#' Expected value of the Binomial distribution
#'
#' @description Expected value of the Binomial distribution with size
#'  \eqn{n}{n} and probability of success \eqn{p}{p}.
#'
#' @template binom-template
#'
#' @export
#'
#' @examples
#'
#' E_binom(size = 3, prob = 0.5)
#'
E_binom <- function(size, prob) {
    stopifnot(
        prob <= 1, prob >= 0,
        size %% 1 == 0, size >= 0
    )

    size * prob
}
