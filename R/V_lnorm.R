#' Variance of the Lognormal distribution
#'
#' @description Variance of the Lognormal distribution with mean
#'  \eqn{\mu}{mu} and variance \eqn{\sigma}{sigma}.
#'
#' @template lnorm-template
#'
#' @export
#'
#' @examples
#'
#' V_lnorm(meanlog = 3, sdlog = 5)
#'
V_lnorm <- function(meanlog, sdlog) {
    stopifnot(sdlog > 0)

    exp(2 * meanlog + (sdlog^2)) *
        (exp(sdlog^2) - 1)
}
