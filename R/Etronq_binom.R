#' Truncated mean of the Binomial distribution
#'
#' @description Truncated mean of the Binomial distribution
#'   with size \eqn{n}{n} and probability of success \eqn{p}{p}.
#'
#' @templateVar d TRUE
#' @templateVar less.than.d TRUE
#' @template binom-template
#'
#' @export
#' @importFrom stats dbinom
#'
#' @examples
#'
#' Etronq_binom(d = 2, size = 3, prob = 0.5)
#' Etronq_binom(d = 0, size = 3, prob = 0.5, less.than.d = FALSE)
#'
Etronq_binom <- function(d, size, prob, less.than.d = TRUE) {
    stopifnot(
        d >= 0, d %% 1 == 0, d <= size,
        prob <= 1, prob >= 0,
        size %% 1 == 0, size >= 0
    )

    k <- 0:size
    fx <- stats::dbinom(x = k, size, prob)

    if (less.than.d) {
        Etronq.binom <- sum((k * fx)[k <= d])
    } else {
        Etronq.binom <- sum((k * fx)[k > d])
    }

    return(Etronq.binom)
}
