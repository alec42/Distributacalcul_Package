#' Expected value of the Gamma distribution
#'
#' @description Expected value of the Gamma distribution with shape
#'  parameter \eqn{\alpha}{alpha} and rate parameter \eqn{\beta}{beta}.
#'
#' @template gamma-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' E_gamma(shape = 3, scale = 4)
#'
#' # With rate parameter
#' E_gamma(shape = 3, rate = 0.25)
#'
E_gamma <- function(shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 0, rate > 0)

    shape / rate
}
