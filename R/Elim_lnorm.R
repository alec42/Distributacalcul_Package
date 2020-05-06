#' Limited Mean of the Lognormal distribution
#'
#' @description Limited expected value of the Lognormal distribution with mean
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
#' Elim_lnorm(d = 2, meanlog = 2, sdlog = 5)
#'
Elim_lnorm <- function(d, meanlog, sdlog) {
    esp <- exp(meanlog + (sdlog^2) / 2)
    phi1 <- (log(d) - meanlog - sdlog^2) / sdlog
    phi2 <-  (log(d) - meanlog) / sdlog
    (esp * pnorm(phi1)) + (d * (pnorm(phi2, lower.tail = F)))
}



