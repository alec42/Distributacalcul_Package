#' Value-at-Risk of the Lognormal distribution
#'
#' @description Value-at-Risk of the Lognormal distribution with mean
#'  \eqn{\mu}{mu} and variance \eqn{\sigma}{sigma}.
#'
#' @templateVar kap TRUE
#' @template lnorm-template
#'
#' @export
#'
#' @examples
#'
#' VaR_lnorm(kap = 0.8, meanlog = 3, sdlog = 5)
#'
VaR_lnorm <- function(kap, meanlog, sdlog) {
    stats::qlnorm(p = kap, meanlog = meanlog, sdlog = sdlog)
}
