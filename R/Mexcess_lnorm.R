#' Mean excess loss of the Lognormal distribution
#'
#' @description Mean excess loss of the Lognormal distribution with mean
#'  \eqn{\mu}{mu} and variance \eqn{\sigma}{sigma}.
#'
#' @templateVar d TRUE
#' @template lnorm-template
#'
#' @export
#' @importFrom stats pnorm
#'
#' @examples
#'
#' Mexcess_lnorm(d = 2, meanlog = 2, sdlog = 5)
#'
Mexcess_lnorm <- function(d, meanlog, sdlog) {
    stopifnot(d >= 0, sdlog > 0)

    E_lnorm(meanlog, sdlog) *
        stats::pnorm(q = log(d) - sdlog^2, mean = meanlog, sd = sdlog, lower.tail = FALSE) /
        stats::pnorm(q = log(d), mean = meanlog, sd = sdlog, lower.tail = FALSE) -
        d
}
