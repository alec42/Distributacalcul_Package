#' Mean excess loss of the Exponential distribution
#'
#' @description Mean excess loss of the Exponential distribution with
#'  rate parameter \eqn{\beta}{beta}.
#'
#' @templateVar d TRUE
#' @template exp-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' Mexcess_exp(d = 2, scale = 4)
#'
#' # With rate parameter
#' Mexcess_exp(d = 5, rate = 0.25)
#'
Mexcess_exp <- function(d, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(d >= 0, rate > 0)

    E_exp(rate)
}
