#' Moment Generating Function of the Exponential distribution
#'
#' @description Moment Generating Function (MGF) of the Exponential
#'  distribution with rate parameter \eqn{\beta}{beta}.
#'
#' @param t t.
#' @template exp-template
#'
#' @export
#'
#' @examples
#'
#' MGF_exp(t = 1, rate = 5)
#'
MGF_exp <- function(t, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(rate > 0, t < rate) # domain for t where non-neg?

    rate / (rate - t)
}
