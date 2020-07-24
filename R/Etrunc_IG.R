#' Truncated mean of the Inverse Gaussian distribution
#'
#' @description Truncated mean of the Inverse Gaussian distribution with
#'  mean \eqn{\mu}{mu} and shape parameter \eqn{\beta}{beta}.
#'
#' @template d-template
#' @template less.than.d-template
#' @template IG-template
#'
#' @export
#' @importFrom stats pnorm
#'
#' @examples
#'
#' Etrunc_IG(d = 2, mean = 2, shape = 5)
#'
Etrunc_IG <- function(d, mean, shape = dispersion * mean^2, dispersion = shape / mean^2, less.than.d = TRUE) {
    stopifnot(d > 0, mean >= 0, shape > 0)

    Etrunc.IG.less.than.d <- d -
        (2 * d - mean) *
        stats::pnorm(q = (d - mean) *
                  sqrt(1 / (shape * d))) -
        (2 * d + mean) *
        exp(2 * mean / shape) *
        stats::pnorm(q = - (d + mean) *
                  sqrt(1 / (shape * d)))

    if (less.than.d) {
        return(Etrunc.IG.less.than.d)
    } else {
        return(mean - Etrunc.IG.less.than.d)
    }
}
