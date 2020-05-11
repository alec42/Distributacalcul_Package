#' Variance of the Lognormal distribution
#'
#' @description Variance of the Lognormal distribution with mean
#'  \eqn{\mu}{mu} and variance \eqn{\sigma}{sigma}.
#'
#' @template lnorm-template
#'
#' @examples
#'
#' V_lnorm(meanlog = 3, sdlog = 5)
#'
#' @export
V_lnorm <- function(meanlog, sdlog) {
    exp(2 * meanlog + sdlog^2) * (exp(sdlog^2) - 1)
}
