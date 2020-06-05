#' Variance of the Poisson distribution
#'
#' @description Variance of the Poisson distribution with rate parameter
#'  \eqn{\lambda}{lambda}.
#'
#' @template pois-template
#'
#' @examples
#'
#' V_pois(lambda = 3)
#'
#' @export
#'
V_pois <- function(lambda) {
    stopifnot(lambda > 0)

    lambda
}
