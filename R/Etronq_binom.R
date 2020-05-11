#' Truncated mean of the Binomial distribution
#'
#' @description Truncated mean of the Binomial distribution
#'   with size \eqn{n}{n} and probability of success \eqn{p}{p}.
#'
#' @templateVar q FALSE
#' @templateVar kappa FALSE
#' @template binom-template
#'
#' @examples
#'
#' Etronq_binom(d = 0, size = 3, prob = 0.5, less.than.d = FALSE)
#' Etronq_binom(d = 2, size = 3, prob = 0.5)
#'
#' @export
#'
Etronq_binom <- function(d, size, prob, less.than.d = TRUE) {
    k <- 0:size
    fx <- dbinom(x = k, size, prob)

    if (less.than.d) {
        Etronq.binom <- sum((k * fx)[k <= d])
    } else {
        Etronq.binom <- sum((k * fx)[k > d])
    }

    return(Etronq.binom)
}
