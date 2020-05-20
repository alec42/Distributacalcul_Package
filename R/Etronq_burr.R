#' Truncated mean of the Burr distribution
#'
#' @description Truncated mean of the Burr distribution with shape parameters
#'  \eqn{\alpha}{alpha} (shape1) and \eqn{\tau}{tau} (shape2) as well as rate
#'  parameter \eqn{\lambda}{lambda}.
#'
#' @templateVar d TRUE
#' @templateVar less.than.d TRUE
#' @template burr-template
#'
#' @export
#' @importFrom stats pbeta
#'
#' @examples
#'
#' # With rate parameter
#' Etronq_burr(d = 2, rate = 2, shape1 = 2, shape2 = 5)
#'
#' # With scale parameter
#' Etronq_burr(d = 2, scale = 0.5, shape1 = 2, shape2 = 5)
#'
#' # Values greater than d
#' Etronq_burr(d = 2, scale = 0.5, shape1 = 2, shape2 = 5, less.than.d = FALSE)
#'
Etronq_burr <- function(d, shape1, shape2, rate = 1 / scale, scale = 1 / rate, less.than.d = TRUE) {
    stopifnot(shape1 > 0, shape2 > 0, rate > 0, d >= 0, shape1 * shape2 > 1)

    if (less.than.d) {
        Etronq.burr <- E_burr(shape1, shape2, rate) *
            stats::pbeta(q = (d^shape2 / (rate + (d^shape2))),
                  shape1 = 1 + 1/shape2,
                  shape2 = shape1 - 1/shape2)
    } else {
        Etronq.burr <- E_burr(shape1, shape2, rate) *
            stats::pbeta(q = (d^shape2 / (rate + (d^shape2))),
                  shape1 = 1 + 1/shape2,
                  shape2 = shape1 - 1/shape2,
                  lower.tail = FALSE)
    }

    return(Etronq.burr)
}
