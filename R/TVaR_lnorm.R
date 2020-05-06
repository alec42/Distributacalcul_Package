#' Tail Value-at-Risk of the Lognormal distribution
#'
#' @description Tail Value-at-Risk of the Lognormal distribution with mean
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
#' TVaR_lnorm(kappa = 0.8, meanlog = 2, sdlog = 5)
#'
TVaR_lnorm <- function(kappa, meanlog, sdlog) {
    phi <- qnorm(kappa) - sdlog
    (E_lnorm(meanlog, sdlog) * pnorm(phi, lower.tail = F)) / (1 - kappa)
}



