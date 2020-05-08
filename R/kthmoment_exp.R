#' kth moment of the Exponential distribution
#'
#' @description kth moment of the Exponential distribution with rate parameter
#'  \eqn{\beta}{beta}.
#'
#' @templateVar k TRUE
#' @template exp-template
#'
#' @export
#'
#' @examples
#'
#' # With scale parameter
#' kthmoment_exp(k = 2, scale = 4)
#'
#' # With rate parameter
#' kthmoment_exp(k = 2, rate = 0.25)
#'
kthmoment_exp <- function(k, rate = 1 / scale, scale = 1 / rate) {
    scale^k * factorial(k)
}
