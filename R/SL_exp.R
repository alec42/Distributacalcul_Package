#' Stop-loss of the Exponential distribution
#'
#' @description Stop-loss of the Exponential distribution with
#'  rate parameter \eqn{\beta}{beta}.
#'
#' @templateVar d TRUE
#' @template exp-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' SL_exp(d = 2, scale = 4)
#'
#' # With rate parameter
#' SL_exp(d = 2, rate = 0.25)
#'
SL_exp <- function(d, rate = 1 / scale, scale = 1 / rate) {
    E_exp(rate) * pexp(q = d, rate, lower.tail = FALSE)
}
