#' Variance of the Logarithmic distribution
#'
#' @description Variance of the Logarithmic distribution with
#'  probability parameter \eqn{\gamma}{gamma}.
#'
#' @template logarithmic-template
#'
#' @export
#'
#' @examples
#'
#' V_logarithmic(prob = 0.50)
#'
V_logarithmic <- function(prob) {
    stopifnot(prob > 0, prob < 1)

    (prob + log(1 - prob)) / ((1 - prob)^2 * (log(1 - prob))^2)
}
