#' Expected value of the Inverse Gaussian distribution
#'
#' @description Expected value of the Inverse Gaussian distribution with
#'  mean \eqn{\mu}{mu} and shape parameter \eqn{\beta}{beta}.
#'
#' @template IG-template
#'
#' @export
#'
#' @examples
#'
#' E_IG(mean = 2, shape = 5)
#'
E_IG <- function(mean, shape = dispersion * mean^2, dispersion = shape / mean^2) {
    stopifnot(mean >= 0
              # , shape >= 0 # not in function
    )

    mean
}
