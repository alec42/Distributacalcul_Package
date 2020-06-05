#' Moment Generating Function of the Poisson distribution
#'
#' @description Moment Generating Function (MGF) of the Poisson
#'    distribution with rate parameter \eqn{\lambda}{lambda}.
#'
#' @param t t
#' @template pois-template
#'
#' @examples
#'
#' MGF_pois(t = 1, lambda = 3)
#'
#' @export
#'
MGF_pois <- function(t, lambda) {
    stopifnot(lambda > 0)

    exp(lambda * (exp(t) - 1))
}
