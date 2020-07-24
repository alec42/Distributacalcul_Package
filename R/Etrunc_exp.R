#' Truncated mean of the Exponential distribution
#'
#' @description Truncated mean of the Exponential distribution with
#'  rate parameter \eqn{\beta}{beta}.
#'
#' @template d-template
#' @template less.than.d-template
#' @template exp-template
#'
#' @export
#' @importFrom stats pexp
#'
#' @examples
#'
#' # With scale parameter
#' Etrunc_exp(d = 2, scale = 4)
#'
#' # With rate parameter
#' Etrunc_exp(d = 2, rate = 0.25, less.than.d = FALSE)
#'
Etrunc_exp <- function(d, rate = 1 / scale, scale = 1 / rate, less.than.d = TRUE) {
    stopifnot(d >= 0, rate > 0)

    if (less.than.d) {
        Etrunc.exp <- E_exp(rate) * stats::pexp(q = d, rate = rate) -
            d * stats::pexp(q = d, rate = rate, lower.tail = FALSE)
    } else {
        Etrunc.exp <- E_exp(rate) * stats::pexp(q = d, rate = rate, lower.tail = FALSE) +
            d * stats::pexp(q = d, rate = rate, lower.tail = FALSE)
    }

    return(Etrunc.exp)
}
