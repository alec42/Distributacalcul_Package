#' kth moment of the Lognormal distribution
#'
#' @description kth moment of the Lognormal distribution with mean
#'  \eqn{\mu}{mu} and variance \eqn{\sigma}{sigma}.
#'
#' @templateVar k TRUE
#' @template lnorm-template
#'
#' @export
#'
#' @examples
#'
#' kthmoment_lnorm(k = 2, meanlog = 3, sdlog = 5)
#'
kthmoment_lnorm <- function(k, meanlog, sdlog) {
    stopifnot(sdlog > 0) # k?

    exp(meanlog * k + k^2 * (sdlog^2) / 2)
}
