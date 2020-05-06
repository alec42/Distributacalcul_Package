#' Value-at-Risk of the Lognormal distribution
#'
#' @description Value-at-Risk of the Lognormal distribution with mean
#'  \eqn{\mu}{mu} and variance \eqn{\sigma}{sigma}.
#'
#' @templateVar q FALSE
#' @templateVar kappa TRUE
#' @template lnorm-template
#'
#' @export
#'
#' @examples
#'
#' VaR_lnorm(kappa = 0.8, meanlog = 3, sdlog = 5)
#'
VaR_lnorm <- function(kappa, meanlog, sdlog) {
    qlnorm(p = kappa, meanlog = meanlog, sdlog = sdlog)
}
