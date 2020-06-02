#' Moment Generating Function of the Binomial distribution
#'
#' @description Moment Generating Function (MGF) of the Binomial
#'  distribution with size \eqn{n}{n} and probability of success \eqn{p}{p}.
#'
#' @param t t
#' @template binom-template
#'
#' @export
#'
#' @examples
#'
#' MGF_binom(t = 1, size = 3, prob = 0.5)
#'
MGF_binom <- function(t, size, prob) {
    stopifnot( # t ?
        prob <= 1, prob >= 0,
        size %% 1 == 0, size >= 0
    )

    (prob * exp(t) + (1 - prob))^size
}
