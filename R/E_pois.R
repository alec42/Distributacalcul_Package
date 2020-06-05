#' Expected value of the Poisson distribution
#'
#' @description Expected value of the Poisson distribution with rate parameter
#'  \eqn{\lambda}{lambda}.
#'
#' @template pois-template
#'
#' @examples
#'
#' E_pois(lambda = 3)
#'
#' @export
#'
E_pois <- function(lambda) {
    stopifnot(lambda > 0)

    lambda
}
