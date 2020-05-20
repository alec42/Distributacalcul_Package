#' Stop-loss of the Inverse Gaussian distribution
#'
#' @description Stop-loss of the Inverse Gaussian distribution with
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
#' SL_IG(d = 2, mean = 2, shape = 5)
#'
SL_IG <- function(d, mean, shape = dispersion * mean^2, dispersion = shape / mean^2) {
    stopifnot(mean >= 0, shape >= 0, d >= 0)

    (mean - d) * stats::pnorm(q = (d - mean) * sqrt(1 / (shape*d)), lower.tail = FALSE) +
    (mean + d) * exp(2*mean / shape) * stats::pnorm(q = - (d + mean) * sqrt(1 / (shape*d)))
}
