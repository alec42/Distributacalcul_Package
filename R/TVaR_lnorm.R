#' Tail Value-at-Risk of the Lognormal distribution
#'
#' @description Tail Value-at-Risk of the Lognormal distribution with mean
#'  \eqn{\mu}{mu} and variance \eqn{\sigma}{sigma}.
#'
#' @templateVar kap TRUE
#' @template lnorm-template
#'
#' @export
#'
#' @examples
#'
#' TVaR_lnorm(kap = 0.8, meanlog = 2, sdlog = 5)
#'
TVaR_lnorm <- function(kap, meanlog, sdlog) {
    stopifnot(kap < 1, kap >= 0, sdlog > 0)

    E_lnorm(meanlog, sdlog) *
        stats::pnorm(q = stats::qnorm(p = kap) - sdlog, lower.tail = FALSE) /
        (1 - kap)
}



