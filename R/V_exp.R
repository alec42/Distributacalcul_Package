#' Variance of the Exponential distribution
#'
#' @description Variance of the Exponential distribution with
#'  rate parameter \eqn{\beta}{beta}.
#'
#' @template exp-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' V_exp(scale = 4)
#'
#' # With rate parameter
#' V_exp(rate = 0.25)
#'
V_exp <- function(rate = 1 / scale, scale = 1 / rate) {
    stopifnot(rate > 0)

    (1/rate)^2
}
