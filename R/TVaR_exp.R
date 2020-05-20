#' Tail Value-at-Risk of the Exponential distribution
#'
#' @description Tail Value-at-Risk of the Exponential distribution with
#'  rate parameter \eqn{\beta}{beta}.
#'
#' @templateVar kap TRUE
#' @template exp-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' TVaR_exp(kap = .99, scale = 4)
#'
#' # With rate parameter
#' TVaR_exp(kap = .99, rate = 0.25)
#'
TVaR_exp <- function(kap, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(kap >= 0, kap < 1, rate > 0)

    VaR_exp(kap = kap, rate) + E_exp(rate)
}
