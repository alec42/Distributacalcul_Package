#' Limited mean of the Normal distribution
#'
#' @description Limited mean of the Normal distribution with mean
#'  \eqn{\mu}{mu} and variance \eqn{\sigma}{sigma}.
#'
#' @templateVar d TRUE
#' @templateVar q FALSE
#' @templateVar kappa FALSE
#' @template norm-template
#'
#' @export
#'
#' @examples
#'
#' Elim_norm(d = 2, mean = 2, sd = 5)
#'
Elim_norm <- function(d, mean = 0, sd = 1) {
    phi1 <- (d - mean) / sd
    fact_norm <- (sd / sqrt(2*pi)) * exp(-phi1^2 / 2)
    mean * pnorm(phi1) - fact_norm + d*(pnorm(phi1, lower.tail = F))
}
