#' Probability Generating Function of the Poisson distribution
#'
#' @description Probability Generating Function (PGF) of the Poisson
#'    distribution with rate parameter \eqn{\lambda}{lambda}.
#'
#' @param t t
#' @template pois-template
#'
#' @examples
#'
#' PGF_pois(t = 1, lambda = 3)
#'
#' @export
#'
PGF_pois <- function(t, lambda) {
    stopifnot(lambda > 0)

    exp(lambda * (t - 1))
}
