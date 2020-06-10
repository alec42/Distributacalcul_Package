#' Density function of the Loglogistic distribution
#'
#' @description Density function of the Loglogistic distribution with shape
#'  parameter \eqn{\tau}{tau} and scale parameter \eqn{\lambda}{lambda}.
#'
#' @template x-template
#' @template loglogistic-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' dllogis(x = 3, shape = 3, scale = 5)
#'
#' # With rate parameter
#' dllogis(x = 3, shape = 3, rate = 0.2)
#'
dllogis <- function(x, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(x >= 0, shape > 1, rate > 0)

    (shape * rate^shape * x^(shape - 1)) / (rate^shape + x^shape)^(2)
}
