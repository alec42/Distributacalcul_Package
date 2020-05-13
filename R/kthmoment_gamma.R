#' kth moment of the Gamma distribution
#'
#' @description kth moment of the Gamma distribution with shape
#'  parameter \eqn{\alpha}{alpha} and rate parameter \eqn{\beta}{beta}.
#'
#' @templateVar k TRUE
#' @template gamma-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' kthmoment_gamma(k = 2, shape = 3, scale = 4)
#'
#' # With rate parameter
#' kthmoment_gamma(k = 2, shape = 3, rate = 0.25)
#'
kthmoment_gamma <- function(k, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 0, rate > 0) # k?

    prod(sapply(0:(k - 1), function(i) (shape + i))) /
        (rate^k)
}
