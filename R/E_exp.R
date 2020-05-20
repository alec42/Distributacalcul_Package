#' Expected value of the Exponential distribution
#'
#' @description Expected value of the Exponential distribution with
#'  rate parameter \eqn{\beta}{beta}.
#'
#' @template exp-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' E_exp(scale = 4)
#'
#' # With rate parameter
#' E_exp(rate = 0.25)
#'
E_exp <- function(rate = 1 / scale, scale = 1 / rate) {
    stopifnot(rate > 0)

    1/rate
}
