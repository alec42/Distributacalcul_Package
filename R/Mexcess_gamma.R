#' Mean excess loss of the Gamma distribution
#'
#' @description Mean excess loss of the Gamma distribution with shape
#'  parameter \eqn{\alpha}{alpha} and rate parameter \eqn{\beta}{beta}.
#'
#' @templateVar d TRUE
#' @template gamma-template
#'
#' @export
#' @importFrom stats pgamma
#'
#' @examples
#'
#' # With scale parameter
#' Mexcess_gamma(d = 2, shape = 3, scale = 4)
#'
#' # With rate parameter
#' Mexcess_gamma(d = 2, shape = 3, rate = 0.25)
#'
Mexcess_gamma <- function(d, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(d >= 0, shape > 0, rate > 0)

    E_gamma(shape, rate) *
        stats::pgamma(q = d, shape = shape + 1, rate = rate, lower.tail = FALSE) /
        stats::pgamma(d, q = shape, shape = rate, lower.tail = FALSE) -
        d
}
