#' Truncated mean of the Inverse Gaussian distribution
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
#' Etronq_IG(d = 2, mean = 2, shape = 5)
#'
Etronq_IG <- function(d, mean, shape = dispersion * mean^2, dispersion = shape / mean^2) {
    stopifnot(d > 0, mean >= 0, shape > 0)

    d -
        (2 * d - mean) *
        stats::pnorm(q = (d - mean) *
                  sqrt(1 / (shape * d))) -
        (2 * d + mean) *
        exp(2 * mean / shape) *
        stats::pnorm(q = - (d + mean) *
                  sqrt(1 / (shape * d)))
}
