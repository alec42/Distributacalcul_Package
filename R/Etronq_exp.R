#' Truncated mean of the Exponential distribution
#'
#' @description Truncated mean of the Exponential distribution with
#'  rate parameter \eqn{\beta}{beta}.
#'
#' @templateVar d TRUE
#' @templateVar less.than.d TRUE
#' @template exp-template
#'
#' @export
#' @importFrom stats pexp
#'
#' @examples
#'
#' # With scale parameter
#' Etronq_exp(d = 2, scale = 4)
#'
#' # With rate parameter
#' Etronq_exp(d = 2, rate = 0.25, less.than.d = FALSE)
#'
Etronq_exp <- function(d, rate = 1 / scale, scale = 1 / rate, less.than.d = TRUE) {
    stopifnot(d >= 0, rate > 0)

    if (less.than.d) {
        Etronq.exp <- E_exp(rate) * stats::pexp(q = d, rate = rate) -
            d * stats::pexp(q = d, rate = rate, lower.tail = FALSE)
    } else {
        Etronq.exp <- E_exp(rate) * stats::pexp(q = d, rate = rate, lower.tail = FALSE) +
            d * stats::pexp(q = d, rate = rate, lower.tail = FALSE)
    }

    return(Etronq.exp)
}
