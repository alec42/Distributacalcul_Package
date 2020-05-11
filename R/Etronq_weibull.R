#' Truncated mean of the Weibull distribution
#'
#' @description Truncated mean of the Weibull distribution with shape parameter
#'  parameter \eqn{\tau}{tau} and rate parameter \eqn{\beta}{beta}.
#'
#' @templateVar d TRUE
#' @templateVar less.than.d TRUE
#' @template weibull-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' Etronq_weibull(d = 2, shape = 2, scale = 5)
#'
#' # With rate parameter
#' Etronq_weibull(d = 2, shape = 2, rate = 0.2)
#'
#' # Mean of values greater than d
#' Etronq_weibull(d = 2, shape = 2, rate = 0.2, less.than.d = FALSE)
#'
Etronq_weibull <- function(d, shape, rate = 1 / scale, scale = 1 / rate, less.than.d = TRUE) {
    stopifnot(shape > 0, rate > 0, d >= 0)

    if (less.than.d) {
        Etronq.weibull <- E_weibull(shape, rate) *
            stats::pgamma(q = d^shape,
                   shape = 1 + 1/shape,
                   scale = rate^shape)
    } else {
        Etronq.weibull <- E_weibull(shape, rate) *
            stats::pgamma(q = d^shape,
                   shape = 1 + 1/shape,
                   scale = rate^shape,
                   lower.tail = FALSE)
    }

    return(Etronq.weibull)
}
