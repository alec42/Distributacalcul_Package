#' Limited mean of the Exponential distribution
#'
#' @description Limited mean of the Exponential distribution with
#'  rate parameter \eqn{\beta}{beta}.
#'
#' @templateVar d TRUE
#' @template exp-template
#'
#' @export
#' @importFrom stats pexp
#'
#' @examples
#'
#' # With scale parameter
#' Elim_exp(d = 2, scale = 4)
#'
#' # With rate parameter
#' Elim_exp(d = 2, rate = 0.25)
#'
Elim_exp <- function(d, rate = 1 / scale, scale = 1 / rate) {
    stopifnot(d >= 0, rate > 0)

    E_exp(rate) * stats::pexp(q = d, rate = rate)
}
