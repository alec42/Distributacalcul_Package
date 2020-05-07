#' Tail Value-at-risk of the Binomial distribution
#'
#' @description Tail Value-at-risk of the Binomial distribution with size
#'  \eqn{n}{n} and probability of success \eqn{p}{p}.
#'
#' @templateVar q FALSE
#' @templateVar kap TRUE
#' @template binom-template
#'
#' @examples
#'
#' TVaR_binom(kap = 0.8, size = 5, prob = 0.2)
#'
#' @export
#'
TVaR_binom <- function(kap, size, prob) {
    k <- 0:size
    fx <- dbinom(x = k, size, prob)
    vark <- qbinom(p = kap, size, prob)

    (Etronq_binom(d = vark, size, prob, less.than.d = FALSE) +
            vark * (pbinom(vark, size, prob) - kap)) /
        (1 - kap)
}
