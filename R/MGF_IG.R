#' Moment Generating Function of the Inverse Gaussian distribution
#'
#' @description Moment Generating Function (MGF) of the Inverse Gaussian
#'   distribution with mean \eqn{\mu}{mu} and shape parameter
#'   \eqn{\beta}{beta}.
#'
#' @param t t
#' @template IG-template
#'
#' @export
#'
#' @examples
#'
#' MGF_IG(t = 1, mean = 2, shape = .5)
#'
MGF_IG <- function(t, mean, shape = dispersion * mean^2, dispersion = shape / mean^2) {
    stopifnot(mean >= 0, shape >= 0, 2 * t * shape <= 1)

    exp((mean/shape) * (1 - sqrt(1 - 2 * shape * t)))
}
