#' Variance of the Loglogistic distribution
#'
#' @description Variance of the Loglogistic distribution with shape parameter
#'  \eqn{\tau}{tau} and scale parameter \eqn{\lambda}{lambda}.
#'
#' @template loglogistic-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' V_llogis(shape = 3, scale = 5)
#'
#' # With rate parameter
#' V_llogis(shape = 3, rate = 0.2)
#'
#' # Equivalently :
#' kthmoment_llogis(k = 2, shape = 3, rate = 0.2) - kthmoment_llogis(k = 1, shape = 3, rate = 0.2)^2
#'
V_llogis <- function(shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape > 2, scale > 0)

    scale^2 * (
        gamma(1 + 2 / shape) * gamma(1 - 2 / shape) -
            (gamma(1 + 1 / shape) * gamma(1 - 1 / shape))^2
    )
}
