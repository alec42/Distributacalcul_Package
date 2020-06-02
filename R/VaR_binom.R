#' Value-at-Risk of the Binomial distribution
#'
#' @description Value-at-Risk of the Binomial distribution with size
#'  \eqn{n}{n} and probability of success \eqn{p}{p}.
#'
#' @note Function VaR_binom is a wrapper for the qbinom function from the
#' stats package.
#'
#' @template binom-template
#' @templateVar kap TRUE
#'
#' @export
#'
#' @importFrom stats qbinom
#'
#' @examples
#'
#' VaR_binom(kap = 0.8, size = 5, prob = 0.2)
#'
VaR_binom <- function(kap, size, prob) {
    stopifnot(
        kap >= 0, kap < 1,
        prob <= 1, prob >= 0,
        size %% 1 == 0, size >= 0
    )

    stats::qbinom(p = kap, size = size, prob = prob)
}
