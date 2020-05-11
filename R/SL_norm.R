#' Stop-loss of the Normal distribution
#'
#' @description Stop-loss of the Normal distribution with mean
#'  \eqn{\mu}{mu} and variance \eqn{\sigma}{sigma}.
#'
#' @templateVar d TRUE
#' @templateVar q FALSE
#' @templateVar kappa FALSE
#' @template norm-template
#'
#' @export
#' @importFrom stats pnorm
#'
#' @examples
#'
#' SL_norm(d = 2, mean = 2, sd = 5)
#'
SL_norm <- function(d, mean = 0, sd = 1) {
    stopifnot(sd > 0)

    phi1 <- (d - mean) / sd
    fact_norm <- (sd / sqrt(2*pi)) * exp(-phi1^2 / 2)
    (mean + d) * stats::pnorm(phi1, lower.tail = F) - fact_norm
}
