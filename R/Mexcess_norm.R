#' Mean excess loss of the Normal distribution
#'
#' @description Mean excess loss of the Normal distribution with mean
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
#' Mexcess_norm(d = 2, mean = 2, sd = 5)
#'
Mexcess_norm <- function(d, mean = 0, sd = 1)
{
    phi1 <- (d - mean) / sd
    fact_norm <- (sd / sqrt(2*pi)) * exp(-phi1^2 / 2)
    mean + d - fact_norm / pnorm(phi1, lower.tail = F)
}
