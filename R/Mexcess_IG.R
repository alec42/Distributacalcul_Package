#' Mean excess loss of the Inverse Gaussian distribution
#'
#' @description Truncated mean of the Inverse Gaussian distribution with
#'  mean \eqn{\mu}{mu} and shape parameter \eqn{\beta}{beta}.
#'
#' @templateVar d TRUE
#' @template IG-template
#'
#' @export
#' @importFrom stats pnorm
#'
#' @examples
#'
#' Mexcess_IG(d = 2, mean = 2, shape = 5)
#'
Mexcess_IG <- function(d, mean, shape = dispersion * mean^2, dispersion = shape / mean^2) {
    stopifnot(d > 0, mean >= 0, shape > 0)

    (
        (mean - d) * stats::pnorm((d - mean) * sqrt(1 / (shape*d)), lower.tail = FALSE) +
        (d + mean) * exp(2*mean / shape) * stats::pnorm(q = -(d + mean) * sqrt(1 / (shape*d)))
    ) /
        (1 - (
              stats::pnorm(q = (d - mean) * sqrt(1 / (shape*d))) +
              exp(2*mean / shape) * stats::pnorm(q = (d + mean) * -sqrt(1 / (shape*d)))
             )
        )
}
