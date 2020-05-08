#' Mean excess loss of the Gamma distribution
#'
#' @description Mean excess loss of the Gamma distribution with shape
#'  parameter \eqn{\alpha}{alpha} and rate parameter \eqn{\beta}{beta}.
#'
#' @templateVar d TRUE
#' @template gamma-template
#'
#' @export
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
    E_gamma(shape, rate) *
        (pgamma(d, shape + 1, rate, lower.tail = F) / pgamma(d, shape, rate, lower.tail = F)) -
        d
}
