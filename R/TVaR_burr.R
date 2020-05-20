#' Tail Value-at-Risk of the Burr distribution
#'
#' @description Tail Value-at-Risk of the Burr distribution with shape parameters
#'  \eqn{\alpha}{alpha} (shape1) and \eqn{\tau}{tau} (shape2) as well as rate
#'  parameter \eqn{\lambda}{lambda}.
#'
#' @templateVar kap TRUE
#' @template burr-template
#'
#' @export
#' @importFrom stats pbeta
#'
#' @examples
#'
#' # With scale parameter
#' TVaR_burr(kap = .8, rate = 2, shape1 = 2, shape2 = 5)
#'
#' # With rate parameter
#' TVaR_burr(kap = .8, scale = 0.5, shape1 = 2, shape2 = 5)
#'
TVaR_burr <- function(kap, shape1, shape2, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(shape1 > 0, shape2 > 0, rate > 0, kap >= 0, kap < 1, shape1 * shape2 > 1)

    vark <- VaR_burr(kap = kap, shape1, shape2, rate)

    (E_burr(shape1, shape2, rate) / (1 - kap)) *
        stats::pbeta(q = (vark^shape2)/(rate + vark^shape2),
                     shape1 = 1 + 1/shape2,
                     shape2 = shape1 - 1/shape2,
                     lower.tail = FALSE)
}
