#' Variance of the Gamma distribution
#'
#' @description Variance of the Gamma distribution with shape
#'  parameter \eqn{\alpha}{alpha} and rate parameter \eqn{\beta}{beta}.
#'
#' @template gamma-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' V_gamma(shape = 3, scale = 4)
#'
#' # With rate parameter
#' V_gamma(shape = 3, rate = 0.25)
#'
V_gamma <- function(shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 0, rate > 0)

    shape / (rate^2)
}
