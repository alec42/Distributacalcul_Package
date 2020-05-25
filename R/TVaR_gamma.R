#' Tail Value-at-Risk of the Gamma distribution
#'
#' @description Tail Value-at-Risk of the Gamma distribution with shape
#'  parameter \eqn{\alpha}{alpha} and rate parameter \eqn{\beta}{beta}.
#'
#' @templateVar kap TRUE
#' @template gamma-template
#'
#' @export
#' @importFrom stats pgamma
#'
#' @examples
#'
#' # With scale parameter
#' TVaR_gamma(kap = .2, shape = 3, scale = 4)
#'
#' # With rate parameter
#' TVaR_gamma(kap = .2, shape = 3, rate = 0.25)
#'
TVaR_gamma <- function(kap, shape, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(kap >= 0, kap < 1, shape > 0, rate > 0)

    (E_gamma(shape, rate) / (1 - kap)) *
        stats::pgamma(q = VaR_gamma(kap, shape, rate), shape = shape + 1, rate = rate, lower.tail = FALSE)
}
