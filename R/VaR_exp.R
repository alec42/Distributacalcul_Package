#' Value-at-Risk of the Exponential distribution
#'
#' @description Value-at-Risk of the Exponential distribution with rate
#'   parameter \eqn{\beta}{beta}.
#'
#' @templateVar kap TRUE
#' @template exp-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' VaR_exp(kap = .99, scale = 4)
#'
#' # With rate parameter
#' VaR_exp(kap = .99, rate = 0.25)
#'
VaR_exp <- function(kap, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(kap >= 0, kap < 1, rate > 0)

    E_exp(rate) * -log(1 - kap)
}
