#' Expected value of the Lognormal distribution
#'
#' @description Expected value of the Lognormal distribution with mean
#'  \eqn{\mu}{mu} and variance \eqn{\sigma}{sigma}.
#'
#' @templateVar q FALSE
#' @templateVar kappa FALSE
#' @template lnorm-template
#'
#' @examples
#'
#' E_lnorm(meanlog = 3, sdlog = 5)
#'
#' @export
#'
E_lnorm <- function(meanlog, sdlog) {
    exp(meanlog + (sdlog^2) / 2)
}


