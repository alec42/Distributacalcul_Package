#' Truncated mean of the Gamma distribution
#'
#' @description Truncated mean of the Gamma distribution with shape
#'  parameter \eqn{\alpha}{alpha} and rate parameter \eqn{\beta}{beta}.
#'
#' @templateVar d TRUE
#' @templateVar less.than.d TRUE
#' @template gamma-template
#'
#' @export
#' @importFrom stats pgamma
#'
#' @examples
#'
#' # With scale parameter
#' Etrunc_gamma(d = 2, shape = 3, scale = 4)
#'
#' # With rate parameter
#' Etrunc_gamma(d = 2, shape = 3, rate = 0.25)
#'
#' # values greather than d
#' Etrunc_gamma(d = 2, shape = 3, rate = 0.25, less.than.d = FALSE)
#'
Etrunc_gamma <- function(d, shape, rate = 1 / scale, scale = 1 / rate, less.than.d = TRUE) {
    stopifnot(d >= 0, shape > 0, rate > 0)

    if (less.than.d) {
        Etrunc.gamma <- E_gamma(shape, rate) * stats::pgamma(q = d, shape = shape + 1, rate = rate)
    } else {
        Etrunc.gamma <- E_gamma(shape, rate) * stats::pgamma(q = d, shape = shape + 1, rate = rate, lower.tail = FALSE)
    }

    return(Etrunc.gamma)
}

