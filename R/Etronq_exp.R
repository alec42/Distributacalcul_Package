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

    if (less.than.d) {
        Etronq.exp <- E_exp(rate) * pexp(q = d, rate) - d * pexp(q = d, rate, lower.tail = F)
    } else {
        Etronq.exp <- E_exp(rate) * pexp(q = d, rate, lower.tail = F) + d * pexp(q = d, rate, lower.tail = F)
    }

    return(Etronq.exp)
}
