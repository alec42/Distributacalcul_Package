#' Moment Generating Function of the Logarithmic distribution
#'
#' @description Moment Generating Function (MGF) of the Logarithmic
#'  distribution with probability parameter \eqn{\gamma}{gamma}.
#'
#' @param t t.
#' @template logarithmic-template
#'
#' @export
#'
#' @examples
#'
#' MGF_logarithmic(t = .2, prob = 0.50)
#'
MGF_logarithmic <- function(t, prob) {
    stopifnot(prob > 0, prob < 1, prob * exp(t) < 1)

    log(1 - prob * exp(t)) / log(1 - prob)
}
