#' Tail Value-at-Risk of the Loglogistic distribution
#'
#' @description Tail Value-at-Risk of the Loglogistic distribution with
#'  shape parameter \eqn{\tau}{tau} and scale parameter
#'  \eqn{\lambda}{lambda}.
#'
#' @templateVar kap TRUE
#' @template loglogistic-template
#'
#' @export
#' @importFrom stats pbeta
#'
#' @examples
#'
#' # With scale parameter
#' TVaR_llogis(kap = 0.8, shape = 3, scale = 5)
#'
#' # With rate parameter
#' TVaR_llogis(kap = 0.8, shape = 3, rate = 0.2)
#'
TVaR_llogis <- function(kap, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(kap >= 0, kap < 1, shape > 1, scale > 0)

    (E_llogis(shape, rate) / (1 - kap)) *
        stats::pbeta(
            q = kap,
            shape1 = 1 + 1/shape,
            shape2 = 1 - 1/shape,
            lower.tail = FALSE
        )
}
