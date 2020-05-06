#' Tail Value-at-risk of the Binomial distribution
#'
#' @description Tail Value-at-risk of the Binomial distribution with size
#'  \eqn{n}{n} and probability of success \eqn{p}{p}.
#'
#' @templateVar q FALSE
#' @template binom-template
#'
#' @examples
#'
#' TVaR_binom(kappa = 0.8, size = 5, prob = 0.2)
#'
#' @export
#'
TVaR_binom <- function(kappa, size, prob) {
    k <- 0:size
    fx <- dbinom(x = k, size, prob)
    vark <- qbinom(p = kappa, size, prob)

    (Etronq_binom(d = vark, size, prob, less.than.d = FALSE) +
            vark * (pbinom(vark, size, prob) - kappa)) /
        (1 - kappa)
}
