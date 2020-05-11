#' Value-at-Risk of the Weibull distribution
#'
#' @description Value-at-Risk of the Weibull distribution with shape
#'  parameter \eqn{\tau}{tau} and rate parameter \eqn{\beta}{beta}.
#'
#' @templateVar kap TRUE
#' @template weibull-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' VaR_weibull(kap = .2, shape = 3, scale = 4)
#'
#' # With rate parameter
#' VaR_weibull(kap = .2, shape = 3, rate = 0.25)
#'
VaR_weibull <- function(kap, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 0, rate > 0, kap <= 1, kap >= 0)

    1 /
        rate *
        (-log(1 - kap))^(1/shape)
}
