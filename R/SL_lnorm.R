#' Stop-loss of the Lognormal distribution
#'
#' @description Stop-loss of the Lognormal distribution with mean
#'  \eqn{\mu}{mu} and variance \eqn{\sigma}{sigma}.
#'
#' @templateVar d TRUE
#' @templateVar q FALSE
#' @templateVar kappa FALSE
#' @template lnorm-template
#'
#' @export
#'
#' @examples
#'
#' SL_lnorm(d = 2, meanlog = 2, sdlog = 5)
#'
SL_lnorm <- function(d, meanlog, sdlog) {
    phi1 <- (log(d) - meanlog - sdlog^2) / sdlog
    phi2 <-  (log(d) - meanlog) / sdlog

    E_lnorm(meanlog, sdlog) * pnorm(phi1, lower.tail = F) -
        d * pnorm(phi2, lower.tail = F)
}



