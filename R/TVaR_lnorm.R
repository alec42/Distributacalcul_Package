#' Tail Value-at-Risk of the Lognormal distribution
#'
#' @description Tail Value-at-Risk of the Lognormal distribution with mean
#'  \eqn{\mu}{mu} and variance \eqn{\sigma}{sigma}.
#'
#' @templateVar q FALSE
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
    phi <- qnorm(kap) - sdlog
    (E_lnorm(meanlog, sdlog) * pnorm(phi, lower.tail = F)) / (1 - kap)
}



