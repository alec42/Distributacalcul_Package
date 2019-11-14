#' Tail Value-at-Risk of the Burr Distribution
#'
#' @description Tail Value-at-Risk of the Burr distribution with shape parameters
#'  \eqn{\alpha}{alpha} (shape1) and \eqn{\tau}{tau} (shape2) as well as rate parameter
#'  \eqn{\lambda}{lambda}.
#'
#' @templateVar kappa TRUE
#' @template burr-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' TVaR_burr(kappa = .8, rate = 2, shape1 = 2, shape2 = 5)
#'
#' # With rate parameter
#' TVaR_burr(kappa = .8, scale = 0.5, shape1 = 2, shape2 = 5)
#'
TVaR_burr <- function(kappa, shape1, shape2, rate = 1 / scale, scale = 1 / rate) {

    vark <- VaR_burr(kappa, shape1, shape2, rate)

    1/((1 - kappa) * gamma(shape1)) *
        (
            (rate^(1/shape2)) *
                gamma(1 + 1/shape2) *
                gamma(shape1 - 1/shape2) *
                pbeta(q = (vark^shape2)/(rate + vark^shape2),
                      shape1 = 1 + 1/shape2,
                      shape2 = shape1 - 1/shape2,
                      lower.tail = F)
        )

}
