#' Truncated mean of the Lognormal distribution
#'
#' @description Truncated mean of the Logormal distribution with mean
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
#' Etronq_lnorm(d = 2, meanlog = 2, sdlog = 5)
#'
Etronq_lnorm <- function(d, meanlog, sdlog) {
    phi1 <- (log(d) - meanlog  - sdlog^2) / sdlog
    exp(meanlog  + (sdlog^2) / 2) * pnorm(phi1)
}



