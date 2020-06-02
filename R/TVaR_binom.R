#' Tail Value-at-Risk of the Binomial distribution
#'
#' @description Tail Value-at-Risk of the Binomial distribution with size
#'  \eqn{n}{n} and probability of success \eqn{p}{p}.
#'
#' @templateVar kap TRUE
#' @template binom-template
#'
#' @export
#' @importFrom stats dbinom qbinom pbinom
#'
#' @examples
#'
#' TVaR_binom(kap = 0.8, size = 5, prob = 0.2)
#'
TVaR_binom <- function(kap, size, prob) {
    stopifnot(
        kap >= 0, kap < 1,
        prob <= 1, prob >= 0,
        size %% 1 == 0, size >= 0
    )

    k <- 0:size
    fx <- stats::dbinom(x = k, size, prob)
    vark <- stats::qbinom(p = kap, size, prob)

    (
        Etronq_binom(d = vark, size, prob, less.than.d = FALSE) +
            vark * (stats::pbinom(q = vark, size = size, prob = prob) - kap)
    ) /
        (1 - kap)
}
