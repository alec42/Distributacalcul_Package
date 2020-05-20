#' Probability Generating Function of the Logarithmic distribution
#'
#' @description Probability Generating Function (PGF) of the Logarithmic
#'  distribution with probability parameter \eqn{\gamma}{gamma}.
#'
#' @param t t.
#' @template logarithmic-template
#'
#' @export
#'
#' @examples
#'
#' PGF_logarithmic(t = .2, prob = 0.50)
#'
PGF_logarithmic <- function(t, prob) {
    stopifnot(prob > 0, prob < 1, prob * t < 1)

    log(1 - prob * t) / log(1 - prob)
}
