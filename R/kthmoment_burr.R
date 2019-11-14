#' kth Moment of the Burr Distribution
#'
#' @description kth-moment of the Burr distribution with shape parameters
#'  \eqn{\alpha}{alpha} (shape1) and \eqn{\tau}{tau} (shape2) as well as rate parameter
#'  \eqn{\lambda}{lambda}.
#'
#' @templateVar k TRUE
#' @templateVar kappa FALSE
#' @template burr-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' kthmoment_burr(k = 1, rate = 2, shape1 = 2, shape2 = 5)
#'
#' # With rate parameter
#' kthmoment_burr(k = 1, scale = 0.5, shape1 = 2, shape2 = 5)
#'
kthmoment_burr <- function(k, shape1, shape2, rate = 1 / scale, scale = 1 / rate)
{
    (1/gamma(shape1)) *
        rate^(k/shape2) *
        gamma(1 + k/shape2) *
        gamma(shape1 - k/shape2)
}
