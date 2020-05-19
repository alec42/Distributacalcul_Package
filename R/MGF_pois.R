#' Moment Generating Function of the Poisson distribution
#'
#' @description Moment Generating Function (PGF) of the Poisson
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
    exp(lambda * (exp(t) - 1))
}
