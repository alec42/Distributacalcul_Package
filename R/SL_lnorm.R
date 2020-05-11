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
#' @importFrom stats pnorm
#'
#' @examples
#'
#' SL_lnorm(d = 2, meanlog = 2, sdlog = 5)
#'
SL_lnorm <- function(d, meanlog, sdlog) {
    stopifnot(sdlog > 0, d >= 0)
    phi1 <- (log(d) - meanlog - sdlog^2) / sdlog
    phi2 <-  (log(d) - meanlog) / sdlog

    E_lnorm(meanlog, sdlog) * stats::pnorm(phi1, lower.tail = F) -
        d * stats::pnorm(phi2, lower.tail = F)
}



