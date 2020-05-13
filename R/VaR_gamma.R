#' Value-at-Risk of the Gamma distribution
#'
#' @description Value-at-Risk of the Gamma distribution with shape parameter
#'  \eqn{\alpha}{alpha} and rate parameter \eqn{\beta}{beta}. Wrapper of
#'  qgamma.
#'
#' @templateVar kap TRUE
#' @template gamma-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' VaR_gamma(kap = .2, shape = 3, scale = 4)
#'
#' # With rate parameter
#' VaR_gamma(kap = .2, shape = 3, rate = 0.25)
#'
VaR_gamma <- function(kap, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 0, rate > 0, kap >= 0, kap <= 1)

    stats::qgamma(p = kap, shape = shape, rate = rate)
}
