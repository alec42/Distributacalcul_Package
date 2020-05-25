#' Value-at-Risk of the Inverse Gaussian distribution
#'
#' @description Value-at-Risk of the Inverse Gaussian distribution with
#'  mean \eqn{\mu}{mu} and shape parameter \eqn{\beta}{beta}.
#'
#' @note Function VaR_IG is a wrapper for the qinvgauss function from the
#' actuar package.
#'
#' @templateVar kap TRUE
#' @template IG-template
#'
#' @export
#' @importFrom actuar qinvgauss
#'
#' @examples
#'
#' VaR_IG(kap = 0.99, mean = 2, shape = 5)
#'
VaR_IG <- function(kap, mean, shape = dispersion * mean^2, dispersion = shape / mean^2) {
    stopifnot(kap >= 0, kap < 1, mean >= 0, shape >= 0)

    actuar::qinvgauss(p = kap, mean = mean, dispersion = dispersion)
}
